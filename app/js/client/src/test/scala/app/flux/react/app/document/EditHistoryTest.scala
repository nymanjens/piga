package app.flux.react.app.document

import hydro.common.GuavaReplacement.Iterables.getOnlyElement
import app.common.testing.JsTestObjects._
import app.common.testing.TestModule
import app.flux.react.app.document.EditHistory.Edit
import app.models.document.Document.DetachedCursor
import app.models.document.Document.DetachedSelection
import app.models.document.DocumentEdit
import app.models.document.DocumentEdit.MaskedTaskUpdate
import app.models.document.Task
import app.models.document.TextWithMarkup
import utest._

import scala.collection.immutable.Seq

object EditHistoryTest extends TestSuite {

  override def tests = TestSuite {
    implicit val clock = new TestModule().fakeClock
    implicit val editHistory = new EditHistory

    "Only updates (non-mergable)" - {
      val abc = newTask("ABC")
      val xyz = newTask("XYZ")

      val updateAbcd = addSameLineUpdateEdit(abc, "ABCD", "D")
      val updateXy = addSameLineUpdateEdit(xyz, "XY")
      val xy = xyz.withAppliedUpdateAndNewUpdateTime(updateXy)
      val updateXyx = addSameLineUpdateEdit(xy, "XYX", "X")

      editHistory.redo() ==> None
      assertEditWithUpdate(editHistory.undo(), updateXyx.reversed)
      assertEditWithUpdate(editHistory.undo(), updateXy.reversed)
      assertEditWithUpdate(editHistory.undo(), updateAbcd.reversed)
      editHistory.undo() ==> None

      assertEditWithUpdate(editHistory.redo(), updateAbcd)

      val abcd = abc.withAppliedUpdateAndNewUpdateTime(updateAbcd)
      val updateZZZ = addSameLineUpdateEdit(abcd, "ZZZ")

      editHistory.redo() ==> None
      assertEditWithUpdate(editHistory.undo(), updateZZZ.reversed)
      assertEditWithUpdate(editHistory.undo(), updateAbcd.reversed)
      editHistory.undo() ==> None
    }

    "Only updates (mergable)" - {
      val abc = newTask("ABC")

      "merged" - {
        val updateAbcd = addSameLineUpdateEdit(abc, "ABCD", "D")
        val abcd = abc.withAppliedUpdateAndNewUpdateTime(updateAbcd)
        val updateAbcdS = addSameLineUpdateEdit(abcd, "ABCD ", " ")
        val abcdS = abcd.withAppliedUpdateAndNewUpdateTime(updateAbcdS)
        val updateAbcdSe = addSameLineUpdateEdit(abcdS, "ABCD E", "E")
        val abcdSe = abcdS.withAppliedUpdateAndNewUpdateTime(updateAbcdSe)
        val updateAbcdSef = addSameLineUpdateEdit(abcdSe, "ABCD EF", "F")

        assertEditWithUpdate(
          editHistory.undo(),
          (updateAbcdS mergedWith updateAbcdSe mergedWith updateAbcdSef).reversed)
        assertEditWithUpdate(editHistory.undo(), updateAbcd.reversed)
      }
      "not merged after undo()" - {
        val updateAbcd = addSameLineUpdateEdit(abc, "ABCD", "D")
        val abcd = abc.withAppliedUpdateAndNewUpdateTime(updateAbcd)
        val updateAbcdS = addSameLineUpdateEdit(abcd, "ABCD ", " ")
        val abcdS = abcd.withAppliedUpdateAndNewUpdateTime(updateAbcdS)
        val updateAbcdSe = addSameLineUpdateEdit(abcdS, "ABCD E", "E")
        assertEditWithUpdate(editHistory.undo(), (updateAbcdS mergedWith updateAbcdSe).reversed)
        val updateAbcdz = addSameLineUpdateEdit(abcd, "ABCDZ", "Z")

        assertEditWithUpdate(editHistory.undo(), updateAbcdz.reversed)
      }
    }

//    "Adds and removes" - {
//      val abc = newTask("ABC")
//      val abcd = newTask("ABCD")
//      val xyz = newTask("XYZ")
//      val xy = newTask("XY")
//      val xyx = newTask("XYX")
//      val xyr = newTask("XYR")
//
//      addSimpleEdit(abc, abcd, "D")
//      addSimpleEdit(xyz, xy)
//      addSimpleEdit(xy, xyx, "X")
//
//      editHistory.redo() ==> None
//      val xy_ = assertEdit(editHistory.undo(), xyx, xy)
//      assertEdit(editHistory.undo(), xy_, xyz)
//      val abc_ = assertEdit(editHistory.undo(), abcd, abc)
//      editHistory.undo() ==> None
//
//      val abcd_ = assertEdit(editHistory.redo(), abc_, abcd)
//
//      addSimpleEdit(abcd_, xyr)
//
//      editHistory.redo() ==> None
//      val abcd__ = assertEdit(editHistory.undo(), xyr, abcd_)
//      assertEdit(editHistory.undo(), abcd__, abc_)
//      editHistory.undo() ==> None
//    }
  }

  private def addSameLineUpdateEdit(originalTask: Task, newContent: String, replacementString: String = "")(
      implicit editHistory: EditHistory): DocumentEdit.MaskedTaskUpdate = {
    val taskUpdate =
      MaskedTaskUpdate.fromFields(originalTask = originalTask, content = TextWithMarkup(newContent))
    editHistory.addEdit(
      documentEdit = DocumentEdit.Reversible(
        removedTasks = Seq(),
        addedTasks = Seq(),
        taskUpdates = Seq(taskUpdate),
      ),
      selectionBeforeEdit = DetachedSelection.singleton(
        DetachedCursor(originalTask.id, originalTask.orderToken, originalTask.contentString.length)),
      selectionAfterEdit = DetachedSelection.singleton(
        DetachedCursor(originalTask.id, originalTask.orderToken, newContent.length)),
      replacementString = replacementString
    )
    taskUpdate
  }

  private def assertEditWithUpdate(edit: Option[Edit], taskUpdate: MaskedTaskUpdate): Unit = {
    assert(edit.isDefined)
    assert(edit.get.documentEdit.removedTasks.isEmpty)
    assert(edit.get.documentEdit.addedTasks.isEmpty)
    assert(edit.get.documentEdit.taskUpdates.size == 1)

    assert(getOnlyElement(edit.get.documentEdit.taskUpdates) == taskUpdate)

    assert(edit.get.selectionBeforeEdit.start.taskId == taskUpdate.taskId)
    assert(edit.get.selectionAfterEdit.start.taskId == taskUpdate.taskId)
  }

  // TODO: REmove
//  private def assertEdit(edit: Option[Edit], removedTask: Task, addedTaskWithNewId: Task): Task = {
//    assert(edit.get.documentEdit.removedTasks == Seq(removedTask))
//
//    val addedTaskInEdit = getOnlyElement(edit.get.documentEdit.addedTasks)
//    assert(addedTaskInEdit.copyWithId(0) == addedTaskWithNewId.copyWithId(0))
//    assert(addedTaskInEdit.id != addedTaskWithNewId.id)
//
//    assert(edit.get.selectionBeforeEdit.start.taskId == removedTask.id)
//    assert(edit.get.selectionAfterEdit.start.taskId == addedTaskInEdit.id)
//
//    addedTaskInEdit
//  }
}
