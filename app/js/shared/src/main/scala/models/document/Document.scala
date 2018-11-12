package models.document

import common.DomNodeUtils.nodeIsLi
import common.OrderToken
import models.access.DbQueryImplicits._
import models.access.{JsEntityAccess, ModelField}
import models.document.Document.{IndexedCursor, IndexedSelection}
import org.scalajs.dom

import scala.annotation.tailrec
import scala.async.Async.{async, await}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

final class Document(val id: Long, val name: String, val orderToken: OrderToken, val tasks: Seq[Task]) {
  require(tasks.sorted == tasks, tasks) // TODD: Remove this check when we're confident that this works

  def updateFromDocumentEntity(documentEntity: DocumentEntity): Document = {
    require(id == documentEntity.id)
    new Document(id = id, name = documentEntity.name, orderToken = documentEntity.orderToken, tasks = tasks)
  }

  def replaced(toReplace: Iterable[Task], toAdd: Iterable[Task]): Document =
    (toReplace.toVector, toAdd.toVector) match {
      case (Seq(replace), Seq(add)) if replace.orderToken == add.orderToken =>
        // Optimization
        val taskIndex = indexOf(replace)
        new Document(id, name, orderToken, tasks.updated(taskIndex, add))

      case (toReplaceSeq, toAddSeq) =>
        val toReplaceSet = toReplaceSeq.toSet
        val newTasks = mutable.Buffer[Task]()
        var toAddSeqIndex = 0
        def nextTaskToAdd: Option[Task] =
          if (toAddSeqIndex < toAddSeq.size) Some(toAddSeq(toAddSeqIndex)) else None

        for {
          t <- tasks
          if !toReplaceSet.contains(t)
        } {
          while (nextTaskToAdd.isDefined && nextTaskToAdd.get < t) {
            newTasks += nextTaskToAdd.get
            toAddSeqIndex += 1
          }
          if (nextTaskToAdd == Some(t)) {
            toAddSeqIndex += 1
          }
          newTasks += t
        }
        while (nextTaskToAdd.isDefined) {
          newTasks += nextTaskToAdd.get
          toAddSeqIndex += 1
        }

        new Document(id, name, orderToken, newTasks.toVector)
    }

  def +(task: Task): Document = replaced(toReplace = Seq(), toAdd = Seq(task))
  def minusTaskWithId(taskId: Long): Document =
    new Document(id, name, orderToken, tasks.filter(_.id != taskId))

  def indexOf(task: Task): Int = {
    def inner(lowerIndex: Int, upperIndex: Int): Int = {
      require(lowerIndex <= upperIndex, s"$task is not in $tasks")
      val index = (upperIndex + lowerIndex) / 2
      tasks(index) match {
        case t if t.id == task.id =>
          require(task == t)
          index
        case t if task.orderToken < t.orderToken  => inner(lowerIndex, upperIndex - 1)
        case t if task.orderToken == t.orderToken => findWithEqualOrderTokenAround(index)
        case _                                    => inner(index + 1, upperIndex)
      }
    }
    def findWithEqualOrderTokenAround(index: Int): Int = {
      var i = index
      while (i > 0 && tasks(i - 1).orderToken == task.orderToken) {
        i -= 1
      }
      while (tasks(i).id != task.id) {
        require(tasks(i).orderToken == task.orderToken, s"$task is not in $tasks")
        i += 1
      }
      i
    }

    inner(0, tasks.length - 1)
  }

  def tasksOption(index: Int): Option[Task] = index match {
    case i if i < 0             => None
    case i if i >= tasks.length => None
    case _                      => Some(tasks(index))
  }

  def toDocumentEntity: DocumentEntity = DocumentEntity(name, orderToken = orderToken, idOption = Some(id))

  case class CollapsedTasksRange(parentSeqIndex: Int, lastChildSeqIndex: Int) {
    def numberOfTasks: Int = lastChildSeqIndex - parentSeqIndex + 1
  }
  def collapsedTasksRange(seqIndex: Int): Option[CollapsedTasksRange] = {
    def findCollapsedParentIndex(seqIndex: Int, currentIndentation: Int): Option[Int] = {
      if (tasks(seqIndex).collapsed) {
        Some(seqIndex)
      } else {
        var parentIndex = seqIndex
        while (parentIndex >= 0 && !(tasks(parentIndex).collapsed &&
                 tasks(parentIndex).indentation < currentIndentation)) {
          parentIndex -= 1
        }
        if (parentIndex == -1) {
          None
        } else {
          Some(parentIndex)
        }
      }
    }
    def findLastChildIndex(seqIndex: Int, parentIndentation: Int): Int = {
      var lastChildIndex = seqIndex
      while (tasksOption(lastChildIndex + 1).isDefined &&
             tasks(lastChildIndex + 1).indentation > parentIndentation) {
        lastChildIndex += 1
      }
      lastChildIndex
    }

    for {
      task <- tasksOption(seqIndex)
      parentIndex <- findCollapsedParentIndex(seqIndex, task.indentation)
    } yield
      CollapsedTasksRange(
        parentIndex,
        findLastChildIndex(seqIndex, parentIndentation = tasks(parentIndex).indentation))
  }

  // **************** Object methods **************** //
  override def equals(o: scala.Any): Boolean = {
    o match {
      case that if this.hashCode != that.hashCode() => false
      case that: Document =>
        this.id == that.id && this.name == that.name && this.orderToken == that.orderToken && this.tasks == that.tasks
      case _ => false
    }
  }
  override lazy val hashCode: Int = {
    var code = 11 + id.hashCode()
    code = code * 7 + name.hashCode()
    code = code * 7 + orderToken.hashCode()
    code = code * 7 + tasks.hashCode()
    code
  }
  override def toString: String = s"Document($id, $name, $tasks)"
}
object Document {

  def fromDocumentEntity(entity: DocumentEntity)(implicit entityAccess: JsEntityAccess): Future[Document] =
    async {
      val tasks = await(
        entityAccess.newQuery[TaskEntity]().filter(ModelField.TaskEntity.documentId === entity.id).data())
      new Document(
        id = entity.id,
        name = entity.name,
        orderToken = entity.orderToken,
        tasks = tasks.map(Task.fromTaskEntity).sorted)
    }

  case class IndexedCursor(seqIndex: Int, offsetInTask: Int) extends Ordered[IndexedCursor] {

    override def compare(that: IndexedCursor): Int = {
      import scala.math.Ordered.orderingToOrdered
      (this.seqIndex, this.offsetInTask) compare ((that.seqIndex, that.offsetInTask))
    }

    def detach(implicit document: Document): DetachedCursor =
      DetachedCursor(task = document.tasks(seqIndex), offsetInTask = offsetInTask)

    def proceedNTasks(n: Int): IndexedCursor = n match {
      case 0 => this
      case _ => IndexedCursor(seqIndex + n, 0)
    }
    def plusOffset(diff: Int): IndexedCursor = IndexedCursor(seqIndex, offsetInTask + diff)
    def minusOffset(diff: Int): IndexedCursor = plusOffset(-diff)

    def plusLines(seqIndexDiff: Int): IndexedCursor = IndexedCursor(seqIndex + seqIndexDiff, offsetInTask)

    def plusOffsetInSeq(diff: Int)(implicit document: Document): IndexedCursor = {
      val tasks = document.tasks
      @tailrec
      def fixOffset(c: IndexedCursor): IndexedCursor = c.offsetInTask match {
        case offset if offset < 0 =>
          if (c.seqIndex == 0) {
            IndexedCursor(0, 0)
          } else {
            fixOffset(IndexedCursor(c.seqIndex - 1, tasks(c.seqIndex - 1).contentString.length + offset + 1))
          }
        case offset if offset > tasks(c.seqIndex).contentString.length =>
          if (c.seqIndex == tasks.length - 1) {
            IndexedCursor(tasks.length - 1, tasks(tasks.length - 1).contentString.length)
          } else {
            fixOffset(IndexedCursor(c.seqIndex + 1, offset - tasks(c.seqIndex).contentString.length - 1))
          }
        case _ => c
      }
      fixOffset(IndexedCursor(seqIndex, offsetInTask + diff))
    }
    def minusOffsetInSeq(diff: Int)(implicit document: Document): IndexedCursor = plusOffsetInSeq(-diff)

    def plusWord(implicit document: Document): IndexedCursor = moveWord(step = 1)
    def minusWord(implicit document: Document): IndexedCursor = moveWord(step = -1)
    private def moveWord(step: Int)(implicit document: Document): IndexedCursor = {
      val result = copy(offsetInTask = {
        val task = document.tasks(seqIndex).contentString
        @tailrec
        def move(offsetInTask: Int, seenWord: Boolean = false): Int = {
          val nextOffset = offsetInTask + step
          if (nextOffset < 0 || nextOffset > task.length) {
            offsetInTask
          } else {
            val currentChar = if (step > 0) task.charAt(offsetInTask) else task.charAt(nextOffset)
            val currentCharIsWord = currentChar.isLetterOrDigit
            if (currentCharIsWord) {
              move(nextOffset, seenWord = true)
            } else {
              if (seenWord) {
                offsetInTask
              } else {
                move(nextOffset, seenWord = false)
              }
            }
          }
        }
        move(offsetInTask)
      })
      if (this == result) {
        // No movement happened --> move to the next/previous line
        plusOffsetInSeq(step)
      } else {
        result
      }
    }

    def toStartOfTask: IndexedCursor = IndexedCursor(seqIndex, offsetInTask = 0)
    def toEndOfTask(implicit document: Document): IndexedCursor =
      IndexedCursor(seqIndex, offsetInTask = document.tasks(seqIndex).contentString.length)
  }
  object IndexedCursor {
    def tupleFromSelection(selection: dom.raw.Selection): IndexedSelection = {
      val anchor = IndexedCursor.fromNode(selection.anchorNode, selection.anchorOffset)
      val focus = IndexedCursor.fromNode(selection.focusNode, selection.focusOffset)
      if (anchor < focus) IndexedSelection(anchor, focus) else IndexedSelection(focus, anchor)
    }

    def atStartOfLine(seqIndex: Int): IndexedCursor = IndexedCursor(seqIndex, 0)
    def atEndOfLine(seqIndex: Int)(implicit document: Document) =
      IndexedCursor(seqIndex, document.tasks(seqIndex).contentString.length)

    private def fromNode(node: dom.raw.Node, offset: Int): IndexedCursor = {
      val parentLi = parentLiElement(node)

      val offsetInTask = {
        val preCursorRange = dom.document.createRange()
        preCursorRange.selectNodeContents(parentLi)
        preCursorRange.setEnd(node, offset)
        preCursorRange.toString.length
      }

      IndexedCursor(seqIndex = parentLi.getAttribute("num").toInt, offsetInTask = offsetInTask)
    }

    private def parentLiElement(node: dom.raw.Node): dom.raw.Element = {
      if (nodeIsLi(node)) {
        node.asInstanceOf[dom.raw.Element]
      } else {
        parentLiElement(node.parentNode)
      }
    }
  }

  case class IndexedSelection(start: IndexedCursor, end: IndexedCursor) {
    require(start <= end)

    def detach(implicit document: Document): DetachedSelection = DetachedSelection(start.detach, end.detach)
    def isSingleton: Boolean = start == end

    def seqIndices: Range = start.seqIndex to end.seqIndex
    def startOffsetInTask(task: Task)(implicit document: Document): Int =
      if (task == document.tasks(start.seqIndex)) start.offsetInTask else 0
    def endOffsetInTask(task: Task)(implicit document: Document): Int =
      if (task == document.tasks(end.seqIndex)) end.offsetInTask else task.contentString.length

    def includeCollapsedChildren(implicit document: Document): IndexedSelection = {
      val task = document.tasks(end.seqIndex)

      var index = end.seqIndex
      if (task.collapsed) {
        while (document.tasksOption(index + 1).isDefined &&
               document.tasks(index + 1).indentation > task.indentation) {
          index += 1
        }
      }
      IndexedSelection(
        start = start,
        end = if (index == end.seqIndex) end else IndexedCursor(index, 0)
      )
    }
  }
  object IndexedSelection {
    def singleton(cursor: IndexedCursor): IndexedSelection = IndexedSelection(start = cursor, end = cursor)
  }

  case class DetachedCursor(task: Task, offsetInTask: Int) {
    def attachToDocument(implicit document: Document): IndexedCursor =
      IndexedCursor(seqIndex = document.indexOf(task), offsetInTask = offsetInTask)
  }
  case class DetachedSelection(start: DetachedCursor, end: DetachedCursor) {
    def isSingleton: Boolean = start == end

    def attachToDocument(implicit document: Document): IndexedSelection =
      IndexedSelection(start = start.attachToDocument, end = end.attachToDocument)
  }
  object DetachedSelection {
    def singleton(cursor: DetachedCursor): DetachedSelection = DetachedSelection(cursor, cursor)
  }
}
