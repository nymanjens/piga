package flux.react.app.desktop

import java.time.Instant

import common.time.Clock
import flux.react.app.desktop.EditHistory.Edit
import flux.react.app.desktop.TaskSequence.DetachedSelection

import scala.collection.immutable.Seq
import scala.collection.mutable

private[desktop] final class EditHistory(implicit clock: Clock) {

  private val edits: mutable.Buffer[Edit] = mutable.Buffer()
  private var nextRedoEditIndex = 0

  // **************** public API **************** //
  def addEdit(removedTasks: Seq[Task],
              addedTasks: Seq[Task],
              selectionBeforeEdit: DetachedSelection,
              selectionAfterEdit: DetachedSelection): Unit = {
    edits.remove(nextRedoEditIndex, edits.length - nextRedoEditIndex)
    edits.append(
      new Edit(
        removedTasks = removedTasks,
        addedTasks = addedTasks,
        selectionBeforeEdit = selectionBeforeEdit,
        selectionAfterEdit = selectionAfterEdit,
        timestamp = clock.nowInstant
      ))
    nextRedoEditIndex = edits.length
  }

  def undo(): Option[Edit] = {
    if (nextRedoEditIndex > 0) {
      val forwardEdit = edits(nextRedoEditIndex - 1)
      nextRedoEditIndex -= 1
      Some(forwardEdit.reverse)
    } else {
      None
    }
  }

  def redo(): Option[Edit] = {
    if (nextRedoEditIndex < edits.length) {
      val edit = edits(nextRedoEditIndex)
      nextRedoEditIndex += 1
      Some(edit)
    } else {
      None
    }
  }
}

private[desktop] object EditHistory {
  private[desktop] class Edit(val removedTasks: Seq[Task],
                              val addedTasks: Seq[Task],
                              val selectionBeforeEdit: DetachedSelection,
                              val selectionAfterEdit: DetachedSelection,
                              timestamp: Instant) {
    def reverse: Edit =
      new Edit(
        removedTasks = addedTasks,
        addedTasks = removedTasks,
        selectionBeforeEdit = selectionAfterEdit,
        selectionAfterEdit = selectionBeforeEdit,
        timestamp = timestamp
      )
  }
}
