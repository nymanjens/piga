package flux.react.app.desktop

import java.time.{Duration, Instant}

import common.GuavaReplacement.Iterables.getOnlyElement
import common.time.Clock
import common.time.JavaTimeImplicits._
import flux.react.app.desktop.EditHistory.Edit
import models.document.Document.DetachedSelection
import models.document.Task

import scala.collection.immutable.Seq
import scala.collection.mutable

private[desktop] final class EditHistory(implicit clock: Clock) {

  private val edits: mutable.Buffer[Edit] = mutable.Buffer()
  private var nextRedoEditIndex = 0
  private var lastEditCanBeMerged: Boolean = false

  // **************** public API **************** //
  def addEdit(removedTasks: Seq[Task],
              addedTasks: Seq[Task],
              selectionBeforeEdit: DetachedSelection,
              selectionAfterEdit: DetachedSelection,
              replacementString: String): Unit = {
    val newEdit = new Edit(
      removedTasks = removedTasks,
      addedTasks = addedTasks,
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

  def undo(): Option[Edit] = {
    if (nextRedoEditIndex > 0) {
      val forwardEdit = edits(nextRedoEditIndex - 1)
      nextRedoEditIndex -= 1
      lastEditCanBeMerged = false
      Some(forwardEdit.reverse)
    } else {
      None
    }
  }

  def redo(): Option[Edit] = {
    if (nextRedoEditIndex < edits.length) {
      val edit = edits(nextRedoEditIndex)
      nextRedoEditIndex += 1
      lastEditCanBeMerged = false
      Some(edit)
    } else {
      None
    }
  }

  private def shouldBeMerged(edit1: Edit, edit2: Edit): Boolean = {
    def hasCollapsedMiddleSelection: Boolean =
      edit1.selectionAfterEdit == edit2.selectionBeforeEdit && edit1.selectionAfterEdit.isCollapsed
    def sameLineIsEdited: Boolean = edit1.addedTasks == edit2.removedTasks
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

private[desktop] object EditHistory {
  private[desktop] case class Edit(removedTasks: Seq[Task],
                                   addedTasks: Seq[Task],
                                   selectionBeforeEdit: DetachedSelection,
                                   selectionAfterEdit: DetachedSelection,
                                   private[EditHistory] val replacementString: String,
                                   private[EditHistory] val timestamp: Instant) {
    def reverse: Edit =
      new Edit(
        removedTasks = addedTasks,
        addedTasks = removedTasks,
        selectionBeforeEdit = selectionAfterEdit,
        selectionAfterEdit = selectionBeforeEdit,
        replacementString = null,
        timestamp = timestamp
      )

    private[EditHistory] def mergedWith(that: Edit): Edit = {
      val overlappingTasks = this.addedTasks.toSet intersect that.removedTasks.toSet
      new Edit(
        removedTasks = this.removedTasks ++ that.removedTasks.filterNot(overlappingTasks),
        addedTasks = that.addedTasks ++ this.addedTasks.filterNot(overlappingTasks),
        selectionBeforeEdit = this.selectionBeforeEdit,
        selectionAfterEdit = that.selectionAfterEdit,
        replacementString = this.replacementString + that.replacementString,
        timestamp = that.timestamp
      )
    }

    private[EditHistory] def addsSingleCharOnSameLine: Boolean =
      addedTasks.size == 1 && replacementString.length == 1 && replacementString.charAt(0) != '\n'
  }
}
