package app.flux.react.app.document

import java.time.Duration
import java.time.Instant

import hydro.common.GuavaReplacement.Iterables.getOnlyElement
import app.flux.react.app.document.EditHistory.Edit
import app.models.document.Document.DetachedCursor
import app.models.document.Document.DetachedSelection
import app.models.document.DocumentEdit
import app.models.document.DocumentEdit.MaskedTaskUpdate
import app.models.document.Task
import hydro.models.modification.EntityModification
import hydro.common.time.Clock
import hydro.common.time.JavaTimeImplicits._

import scala.collection.immutable.Seq
import scala.collection.mutable

private[document] final class EditHistory(implicit clock: Clock) {

  private val edits: mutable.Buffer[Edit] = mutable.Buffer()
  private var nextRedoEditIndex: Int = 0
  private var lastEditCanBeMerged: Boolean = false

  // **************** public API **************** //
  def addEdit(documentEdit: DocumentEdit.Reversible,
              selectionBeforeEdit: DetachedSelection,
              selectionAfterEdit: DetachedSelection,
              replacementString: String): Unit = {
    val newEdit = Edit(
      documentEdit = documentEdit,
      selectionBeforeEdit = selectionBeforeEdit,
      selectionAfterEdit = selectionAfterEdit,
      replacementString = replacementString,
      timestamp = clock.nowInstant
    )

    if (lastEditCanBeMerged && shouldBeMerged(edits.last, newEdit)) {
      edits.update(nextRedoEditIndex - 1, edits.last mergedWith newEdit)
    } else {
      edits.remove(nextRedoEditIndex, edits.length - nextRedoEditIndex)
      edits.append(newEdit)
      nextRedoEditIndex = edits.length
      lastEditCanBeMerged = newEdit.addsSingleCharOnSameLine
    }
  }

  def canUndo: Boolean = nextRedoEditIndex > 0
  def canRedo: Boolean = nextRedoEditIndex < edits.length

  def undo(): Option[Edit] = {
    if (nextRedoEditIndex > 0) {
      def getEdit() = edits(nextRedoEditIndex - 1).reversed
      randomizeIdsInHistory(getEdit().documentEdit.addedTasks.map(_.id))
      val newEdit = getEdit()

      nextRedoEditIndex -= 1
      lastEditCanBeMerged = false

      Some(newEdit)
    } else {
      None
    }
  }

  def redo(): Option[Edit] = {
    if (nextRedoEditIndex < edits.length) {
      def getEdit() = edits(nextRedoEditIndex)
      randomizeIdsInHistory(getEdit().documentEdit.addedTasks.map(_.id))
      val newEdit = getEdit()

      nextRedoEditIndex += 1
      lastEditCanBeMerged = false
      Some(newEdit)
    } else {
      None
    }
  }

  private def randomizeIdsInHistory(oldIds: Seq[Long]): Unit = {
    def updateTaskIdsInHistory(oldId: Long, newId: Long): Unit = {
      def updateTaskId(task: Task): Task = if (task.id == oldId) task.copyWithId(newId) else task
      def updateTaskId2(update: MaskedTaskUpdate): MaskedTaskUpdate =
        if (update.taskId == oldId) update.copy(taskId = newId) else update
      def updateTaskIdsInSeq(tasks: Seq[Task]): Seq[Task] = tasks.map(updateTaskId)
      def updateTaskIdsInUpdates(tasks: Seq[MaskedTaskUpdate]): Seq[MaskedTaskUpdate] =
        tasks.map(updateTaskId2)
      def updateTaskIdsInCursor(cursor: DetachedCursor): DetachedCursor =
        cursor.copy(taskId = if (cursor.taskId == oldId) newId else cursor.taskId)
      def updateTaskIdsInSelection(selection: DetachedSelection): DetachedSelection =
        DetachedSelection(
          start = updateTaskIdsInCursor(selection.start),
          end = updateTaskIdsInCursor(selection.end))

      for ((edit, i) <- edits.zipWithIndex) {
        edits.update(
          i,
          edit.copy(
            documentEdit = DocumentEdit.Reversible(
              addedTasks = updateTaskIdsInSeq(edit.documentEdit.addedTasks),
              removedTasks = updateTaskIdsInSeq(edit.documentEdit.removedTasks),
              taskUpdates = updateTaskIdsInUpdates(edit.documentEdit.taskUpdates),
            ),
            selectionBeforeEdit = updateTaskIdsInSelection(edit.selectionBeforeEdit),
            selectionAfterEdit = updateTaskIdsInSelection(edit.selectionAfterEdit)
          )
        )
      }
    }

    for (id <- oldIds) {
      updateTaskIdsInHistory(oldId = id, newId = EntityModification.generateRandomId())
    }
  }

  private def shouldBeMerged(edit1: Edit, edit2: Edit): Boolean = {
    def hasCollapsedMiddleSelection: Boolean =
      edit1.selectionAfterEdit == edit2.selectionBeforeEdit && edit1.selectionAfterEdit.isSingleton
    def sameLineIsEdited: Boolean =
      edit1.documentEdit.taskUpdates.map(_.taskId) == edit2.documentEdit.taskUpdates.map(_.taskId)
    def tooMuchTimeBetween: Boolean =
      Duration.between(edit1.timestamp, edit2.timestamp) > Duration.ofSeconds(3)
    def isCombiningWord: Boolean = {
      val newChar = getOnlyElement(edit2.replacementString.toVector)
      if (edit1.replacementString.last.isLetterOrDigit) {
        newChar.isLetterOrDigit
      } else {
        true
      }
    }

    hasCollapsedMiddleSelection && sameLineIsEdited && edit2.addsSingleCharOnSameLine && !tooMuchTimeBetween && isCombiningWord
  }
}

private[document] object EditHistory {

  private[document] case class Edit(documentEdit: DocumentEdit.Reversible,
                                    selectionBeforeEdit: DetachedSelection,
                                    selectionAfterEdit: DetachedSelection,
                                    private[EditHistory] val replacementString: String,
                                    private[EditHistory] val timestamp: Instant) {
    def reversed: Edit =
      Edit(
        documentEdit = documentEdit.reversed,
        selectionBeforeEdit = selectionAfterEdit,
        selectionAfterEdit = selectionBeforeEdit,
        replacementString = null,
        timestamp = timestamp
      )

    private[EditHistory] def mergedWith(that: Edit): Edit = {
      Edit(
        documentEdit = this.documentEdit mergedWith that.documentEdit,
        selectionBeforeEdit = this.selectionBeforeEdit,
        selectionAfterEdit = that.selectionAfterEdit,
        replacementString = this.replacementString + that.replacementString,
        timestamp = that.timestamp
      )
    }

    private[EditHistory] def addsSingleCharOnSameLine: Boolean = {
      val noAdds = documentEdit.addedTasks.isEmpty
      val noRemovals = documentEdit.removedTasks.isEmpty
      val singleUpdate = documentEdit.taskUpdates.size == 1
      def eligibleReplacementString = replacementString.length == 1 && replacementString.charAt(0) != '\n'

      noAdds && noRemovals && singleUpdate && eligibleReplacementString
    }
  }
}
