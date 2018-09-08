package models.document

import common.DomNodeUtils.nodeIsLi
import models.access.DbQueryImplicits._
import models.access.{JsEntityAccess, ModelField}
import org.scalajs.dom

import scala.annotation.tailrec
import scala.async.Async.{async, await}
import scala.collection.immutable.Seq
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

final class Document(val id: Long, val name: String, val tasks: Seq[Task]) {
  require(tasks.sorted == tasks, tasks) // TODD: Remove this check when we're confident that this works

  def replaced(toReplace: Iterable[Task], toAdd: Iterable[Task]): Document =
    (toReplace.toVector, toAdd.toVector) match {
      case (Seq(replace), Seq(add)) if replace.orderToken == add.orderToken =>
        // Optimization
        val taskIndex = indexOf(replace)
        new Document(id, name, tasks.updated(taskIndex, add))

      case (toReplaceSeq, toAddSeq) =>
        val toReplaceSet = toReplaceSeq.toSet
        new Document(id, name, tasks.flatMap {
          case task if task == toReplaceSeq.head  => toAddSeq
          case task if toReplaceSet contains task => Seq()
          case task                               => Seq(task)
        })
    }

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

  def toDocumentEntity: DocumentEntity = DocumentEntity(name, idOption = Some(id))

  // **************** Object methods **************** //
  override def equals(o: scala.Any): Boolean = {
    o match {
      case that if this.hashCode != that.hashCode() => false
      case that: Document                           => this.id == that.id && this.name == that.name && this.tasks == that.tasks
      case _                                        => false
    }
  }
  override lazy val hashCode: Int = {
    var code = 11 + id.hashCode()
    code = code * 7 + name.hashCode()
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
      new Document(id = entity.id, name = entity.name, tasks = tasks.map(Task.fromTaskEntity).sorted)
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
    def isCollapsed: Boolean = start == end
  }
  object IndexedSelection {
    def collapsed(cursor: IndexedCursor): IndexedSelection = IndexedSelection(start = cursor, end = cursor)
  }

  case class DetachedCursor(task: Task, offsetInTask: Int) {
    def attachToDocument(implicit document: Document): IndexedCursor =
      IndexedCursor(seqIndex = document.indexOf(task), offsetInTask = offsetInTask)
  }
  case class DetachedSelection(start: DetachedCursor, end: DetachedCursor) {
    def isCollapsed: Boolean = start == end

    def attachToDocument(implicit document: Document): IndexedSelection =
      IndexedSelection(start = start.attachToDocument, end = end.attachToDocument)
  }
}
