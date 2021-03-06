package app.models.document

import org.scalajs.dom.console
import java.util.Objects

import app.models.access.ModelFields
import app.models.document.Document.IndexedSelection
import hydro.common.DomNodeUtils.nodeIsLi
import hydro.common.GuavaReplacement.Iterables.getOnlyElement
import hydro.common.OrderToken
import hydro.models.access.DbQueryImplicits._
import hydro.models.access.JsEntityAccess
import org.scalajs.dom

import scala.annotation.tailrec
import scala.async.Async.async
import scala.async.Async.await
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

final class Document(val id: Long, val name: String, val tasks: Seq[Task]) {

  def withAppliedEdit(edit: DocumentEdit.WithUpdateTimes): Document =
    new Document(
      id,
      name,
      tasks = {
        def quickUpdate(taskIndex: Int, taskUpdate: Task) = {
          // Optimization
          val oldTask = tasks(taskIndex)
          tasks.updated(taskIndex, oldTask mergedWith taskUpdate)
        }
        def comprehensiveUpdate(edit: DocumentEdit.WithUpdateTimes) = {
          def maybeApplyUpdate(oldTask: Task): Task = {
            edit.taskUpdatesById.get(oldTask.id) match {
              case Some(taskUpdate) => oldTask mergedWith taskUpdate
              case None             => oldTask
            }
          }

          val addedTasksMap = mutable.Map(
            edit.addedTasks
              .filterNot(task => edit.removedTasksIds contains task.id)
              .map(task => task.id -> maybeApplyUpdate(task)): _*
          )
          val newTasks = mutable.Buffer[Task]()

          for {
            task <- tasks
            if !edit.removedTasksIds.contains(task.id)
          } {
            newTasks += maybeApplyUpdate(task)
            addedTasksMap.remove(task.id) // Don't add already present tasks
          }
          newTasks ++= addedTasksMap.values

          newTasks.toVector.sorted
        }

        if (edit.removedTasksIds.isEmpty && edit.addedTasks.isEmpty && edit.taskUpdates.size == 1) {
          val update = getOnlyElement(edit.taskUpdates)
          maybeIndexOf(update.id, orderTokenHint = update.orderToken) match {
            case Some(taskIndex) if tasks(taskIndex).orderToken == update.orderToken =>
              quickUpdate(taskIndex, update)
            case _ => comprehensiveUpdate(edit)
          }
        } else {
          comprehensiveUpdate(edit)
        }
      },
    )

  def updateFromDocumentEntity(documentEntity: DocumentEntity): Document = {
    require(id == documentEntity.id)
    new Document(id = id, name = documentEntity.name, tasks = tasks)
  }

  def maybeIndexOf(taskId: Long, orderTokenHint: OrderToken = null): Option[Int] = {
    def indexOfViaOrderToken(): Option[Int] = {
      def findWithEqualOrderTokenAround(index: Int): Option[Int] = {
        var i = index
        while (i > 0 && tasks(i - 1).orderToken == orderTokenHint) {
          i -= 1
        }
        while (tasks(i).id != taskId && tasks(i).orderToken == orderTokenHint) {
          i += 1
        }
        if (tasks(i).id == taskId) Some(i) else None
      }

      def inner(lowerIndex: Int, upperIndex: Int): Option[Int] = {
        if (lowerIndex > upperIndex) {
          None
        } else {
          val index = (upperIndex + lowerIndex) / 2
          tasks(index) match {
            case t if t.id == taskId => Some(index)
            case t if orderTokenHint != null && orderTokenHint < t.orderToken =>
              inner(lowerIndex, upperIndex - 1)
            case t if orderTokenHint == t.orderToken => findWithEqualOrderTokenAround(index)
            case _                                   => inner(index + 1, upperIndex)
          }
        }
      }

      inner(0, tasks.length - 1)
    }

    def indexOfViaIdOnly(): Option[Int] = tasks.toStream.map(_.id).indexOf(taskId) match {
      case -1    => None
      case index => Some(index)
    }

    indexOfViaOrderToken() orElse indexOfViaIdOnly()
  }

  def tasksOption(index: Int): Option[Task] = index match {
    case i if i < 0             => None
    case i if i >= tasks.length => None
    case _                      => Some(tasks(index))
  }

  /**
   * Returns the task at the given index, only if it exists and is not the child of a collapsed task.
   *
   * @param minExpandedIndentation The minimum indentation of which the caller is certain that it is not collapsed
   */
  def visibleTaskOption(index: Int, minExpandedIndentation: Int = -1): Option[Task] = {
    tasksOption(index) filter { task =>
      val ancestors =
        for {
          indentation <- ((task.indentation - 1) to (minExpandedIndentation + 1) by -1).toStream
          parentIndex <- findRootParentIndex(index, rootParentIndentation = indentation)
        } yield tasks(parentIndex)

      ancestors.forall(!_.collapsed)
    }
  }

  def tasksIn(selection: IndexedSelection): Seq[Task] = for (i <- selection.seqIndices) yield tasks(i)

  def toDocumentEntity: DocumentEntity = DocumentEntity(name, idOption = Some(id))

  case class FamilyTreeRange(parentSeqIndex: Int, lastChildSeqIndex: Int) {
    def numberOfTasks: Int = lastChildSeqIndex - parentSeqIndex + 1
  }
  def familyTreeRange(anyMemberSeqIndex: Int, rootParentIndentation: Int): Option[FamilyTreeRange] = {
    def findLastChildIndex(seqIndex: Int, parentIndentation: Int): Int = {
      var lastChildIndex = seqIndex
      while (
        tasksOption(lastChildIndex + 1).isDefined &&
        tasks(lastChildIndex + 1).indentation > parentIndentation
      ) {
        lastChildIndex += 1
      }
      lastChildIndex
    }

    for {
      task <- tasksOption(anyMemberSeqIndex)
      parentIndex <- findRootParentIndex(anyMemberSeqIndex, rootParentIndentation = rootParentIndentation)
    } yield FamilyTreeRange(
      parentIndex,
      findLastChildIndex(anyMemberSeqIndex, parentIndentation = tasks(parentIndex).indentation),
    )
  }

  def findParentIndex(seqIndex: Int): Option[Int] = {
    findRootParentIndex(seqIndex, rootParentIndentation = tasks(seqIndex).indentation - 1)
  }

  private def findRootParentIndex(seqIndex: Int, rootParentIndentation: Int): Option[Int] = {
    var result = seqIndex
    while (result >= 0 && tasks(result).indentation > rootParentIndentation) {
      result -= 1
    }
    result match {
      case -1                                                         => None
      case index if tasks(index).indentation == rootParentIndentation => Some(index)
      case _                                                          => None
    }
  }

  // **************** Object methods **************** //
  override def equals(o: scala.Any): Boolean = {
    o match {
      case that: Document =>
        this.id == that.id && this.name == that.name && this.tasks == that.tasks
      case _ => false
    }
  }
  override def hashCode: Int = Objects.hash(id.asInstanceOf[java.lang.Long], name, tasks)
  override def toString: String = s"Document($id, $name, $tasks)"
}
object Document {

  val nullInstance: Document = new Document(id = -1, name = "", tasks = Seq())

  def fromDocumentEntity(entity: DocumentEntity)(implicit entityAccess: JsEntityAccess): Future[Document] =
    async {
      val tasks = await(
        entityAccess.newQuery[TaskEntity]().filter(ModelFields.TaskEntity.documentId === entity.id).data()
      )
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

    def plusTasks(seqIndexDiff: Int): IndexedCursor = IndexedCursor(seqIndex + seqIndexDiff, offsetInTask)

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
            IndexedCursor(tasks.length - 1, tasks.last.contentString.length)
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
        // No movement happened --> move to the next/previous task
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
    def atStartOfTask(seqIndex: Int): IndexedCursor = IndexedCursor(seqIndex, 0)
    def atEndOfTask(seqIndex: Int)(implicit document: Document) =
      IndexedCursor(seqIndex, document.tasks(seqIndex).contentString.length)
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

    def includeFullTasks()(implicit document: Document): IndexedSelection = {
      IndexedSelection(start = start.toStartOfTask, end = end.toEndOfTask)
    }

    def includeChildren(collapsedOnly: Boolean = false)(implicit document: Document): IndexedSelection = {
      val task = document.tasks(end.seqIndex)

      var index = end.seqIndex
      if (!collapsedOnly || task.collapsed) {
        while (
          document.tasksOption(index + 1).isDefined &&
          document.tasks(index + 1).indentation > task.indentation
        ) {
          index += 1
        }
      }
      IndexedSelection(
        start = start,
        end = if (index == end.seqIndex) end else IndexedCursor(index, 0),
      )
    }
  }
  object IndexedSelection {

    val nullInstance: IndexedSelection = IndexedSelection(IndexedCursor(0, 0), IndexedCursor(0, 0))

    def singleton(cursor: IndexedCursor): IndexedSelection = IndexedSelection(start = cursor, end = cursor)
    def atStartOfTask(index: Int): IndexedSelection =
      IndexedSelection.singleton(IndexedCursor.atStartOfTask(index))

    def tupleFromSelection(selection: dom.raw.Selection): Option[IndexedSelection] = {
      for {
        anchor <- cursorFromNode(selection.anchorNode, selection.anchorOffset)
        focus <- cursorFromNode(selection.focusNode, selection.focusOffset)
      } yield {
        if (anchor < focus) IndexedSelection(anchor, focus) else IndexedSelection(focus, anchor)
      }
    }

    private def cursorFromNode(node: dom.raw.Node, offset: Int): Option[IndexedCursor] = {
      try {
        val parentLi = parentLiElement(node)
        val offsetInTask = {
          val preCursorRange = dom.document.createRange()
          preCursorRange.selectNodeContents(parentLi)
          preCursorRange.setEnd(node, offset)
          preCursorRange.toString.length
        }

        Some(IndexedCursor(seqIndex = parentLi.getAttribute("num").toInt, offsetInTask = offsetInTask))
      } catch {
        case e: NoParentLiException =>
          console.log("Could not find parent li of node", node, " (offset = ", offset, ")")
          e.printStackTrace()
          None
      }
    }

    private def parentLiElement(node: dom.raw.Node): dom.raw.Element = {
      if (node == null) {
        throw new NoParentLiException()
      } else if (nodeIsLi(node)) {
        node.asInstanceOf[dom.raw.Element]
      } else {
        parentLiElement(node.parentNode)
      }
    }

    private final class NoParentLiException() extends Exception
  }

  case class DetachedCursor(taskId: Long, originalOrderToken: OrderToken, offsetInTask: Int) {
    def attachToDocument(implicit document: Document): IndexedCursor =
      IndexedCursor(
        seqIndex = document.maybeIndexOf(taskId, orderTokenHint = originalOrderToken) getOrElse {
          println(s"  Warning: Could not find task in document: taskId = $taskId")
          0
        },
        offsetInTask = offsetInTask,
      )
  }
  object DetachedCursor {
    def apply(task: Task, offsetInTask: Int): DetachedCursor =
      DetachedCursor(taskId = task.id, originalOrderToken = task.orderToken, offsetInTask = offsetInTask)
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
