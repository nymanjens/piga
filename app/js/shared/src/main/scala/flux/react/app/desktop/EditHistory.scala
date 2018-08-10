package flux.react.app.desktop

import java.time.Instant

import common.DomNodeUtils._
import common.GuavaReplacement.Splitter
import common.I18n
import common.LoggingUtils.{LogExceptionsCallback, logExceptions}
import common.time.Clock
import flux.react.router.RouterContext
import japgolly.scalajs.react._
import japgolly.scalajs.react.raw.SyntheticKeyboardEvent
import japgolly.scalajs.react.vdom.PackageBase.VdomAttr
import japgolly.scalajs.react.vdom.html_<^._
import jsfacades.escapeHtml
import models.access.EntityAccess
import org.scalajs.dom
import org.scalajs.dom.console

import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.scalajs.js

private[desktop] final class EditHistory(implicit clock: Clock) {

  private val edits: mutable.Buffer[Edit] = mutable.Buffer()
  private var nextRedoEditIndex = 0

  // **************** public API **************** //
  def addEdit(removedTasks: Seq[Task],
              addedTasks: Seq[Task],
              selectionBeforeEdit: TaskSeqSelection,
              selectionAfterEdit: TaskSeqSelection)(implicit tasks: TaskSequence): Unit = {
    edits.remove(nextRedoEditIndex, edits.length - nextRedoEditIndex)
    edits.append(
      new Edit(
        removedTasks = removedTasks,
        addedTasks = addedTasks,
        absoluteSelectionBeforeEdit = AbsoluteSelection.fromSeqSelection(selectionBeforeEdit),
        absoluteSelectionAfterEdit = AbsoluteSelection.fromSeqSelection(selectionAfterEdit),
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

  // **************** inner types **************** //
  private case class AbsoluteCursor(task: Task, offsetInTask: Int) {
    def toSeqCursor(implicit tasks: TaskSequence): TaskSeqCursor =
      TaskSeqCursor(seqIndex = tasks.indexOf(task), offsetInTask = offsetInTask)
  }
  private object AbsoluteCursor {
    def fromSeqCursor(cursor: TaskSeqCursor)(implicit tasks: TaskSequence): AbsoluteCursor =
      AbsoluteCursor(task = tasks(cursor.seqIndex), offsetInTask = cursor.offsetInTask)
  }

  private case class AbsoluteSelection(start: AbsoluteCursor, end: AbsoluteCursor) {
    def toSeqSelection(implicit tasks: TaskSequence): TaskSeqSelection =
      TaskSeqSelection(start = start.toSeqCursor, end = end.toSeqCursor)
  }
  private object AbsoluteSelection {
    def fromSeqSelection(selection: TaskSeqSelection)(implicit tasks: TaskSequence): AbsoluteSelection =
      AbsoluteSelection(
        start = AbsoluteCursor.fromSeqCursor(selection.start),
        end = AbsoluteCursor.fromSeqCursor(selection.end))
  }

  private[desktop] class Edit(val removedTasks: Seq[Task],
                              val addedTasks: Seq[Task],
                              absoluteSelectionBeforeEdit: AbsoluteSelection,
                              absoluteSelectionAfterEdit: AbsoluteSelection,
                              timestamp: Instant) {
    def reverse: Edit =
      new Edit(
        removedTasks = addedTasks,
        addedTasks = removedTasks,
        absoluteSelectionBeforeEdit = absoluteSelectionAfterEdit,
        absoluteSelectionAfterEdit = absoluteSelectionBeforeEdit,
        timestamp = timestamp
      )

    def selectionBeforeEdit(implicit tasks: TaskSequence): TaskSeqSelection =
      absoluteSelectionBeforeEdit.toSeqSelection
    def selectionAfterEdit(implicit tasks: TaskSequence): TaskSeqSelection =
      absoluteSelectionAfterEdit.toSeqSelection
  }
}
