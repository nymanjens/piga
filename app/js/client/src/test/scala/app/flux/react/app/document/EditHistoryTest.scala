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
import app.models.document.TextWithMarkup.Formatting
import app.models.user.User
import hydro.common.OrderToken
import utest._

import scala.collection.immutable.Seq

object EditHistoryTest extends TestSuite {

  override def tests = TestSuite {
    implicit val fakeClock = new TestModule().fakeClock
    implicit val testUser = new TestModule().testUser
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
          (updateAbcdS mergedWith updateAbcdSe mergedWith updateAbcdSef).reversed,
        )
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

    "Adds and removes" - {
      val taskAbc = newTask("ABC")
      val task2 = newTask("DEF")
      val task3 = newTask("GHI")
      val task4 = newTask("JKL")
      val task5 = newTask("MNO")
      val task6 = newTask("PQR")

      addEdit(addedTask = taskAbc)
      val updateAbcd = addSameLineUpdateEdit(taskAbc, "ABCD", "D")
      val taskAbcd = taskAbc.withAppliedUpdateAndNewUpdateTime(updateAbcd)
      addEdit(addedTask = task2)
      addEdit(addedTask = task3)
      addEdit(removedTask = task3)
      addEdit(removedTask = taskAbcd)

      editHistory.redo() ==> None
      val taskAbcd_ = assertEdit(editHistory.undo(), addedTaskWithNewId = taskAbcd)
      val task3_ = assertEdit(editHistory.undo(), addedTaskWithNewId = task3)
      assertEdit(editHistory.undo(), removedTask = task3_)
      assertEdit(editHistory.undo(), removedTask = task2)
      val updateAbcd_ = updateAbcd.copy(taskId = taskAbcd_.id)
      assertEditWithUpdate(editHistory.undo(), updateAbcd_.reversed)
      val taskAbc_ = taskAbc.copyWithId(taskAbcd_.id)
      assertEdit(editHistory.undo(), removedTask = taskAbc_)
      editHistory.undo() ==> None

      val taskAbc__ = assertEdit(editHistory.redo(), addedTaskWithNewId = taskAbc_)
      val updateAbcd__ = updateAbcd_.copy(taskId = taskAbc__.id)
      assertEditWithUpdate(editHistory.redo(), updateAbcd__)
    }
  }

  private def addSameLineUpdateEdit(
      originalTask: Task,
      newContent: String,
      replacementString: String = "",
  )(implicit
      editHistory: EditHistory,
      user: User,
  ): DocumentEdit.MaskedTaskUpdate = {
    val taskUpdate =
      MaskedTaskUpdate.fromFields(originalTask = originalTask, content = textWithMarkup(newContent))
    editHistory.addEdit(
      documentId = 19191,
      documentEdit = DocumentEdit.Reversible(
        removedTasks = Seq(),
        addedTasks = Seq(),
        taskUpdates = Seq(taskUpdate),
      ),
      selectionBeforeEdit = DetachedSelection.singleton(
        DetachedCursor(originalTask.id, originalTask.orderToken, originalTask.contentString.length)
      ),
      selectionAfterEdit = DetachedSelection.singleton(
        DetachedCursor(originalTask.id, originalTask.orderToken, newContent.length)
      ),
      replacementString = replacementString,
    )
    taskUpdate
  }

  private def addEdit(removedTask: Task = null, addedTask: Task = null)(implicit
      editHistory: EditHistory
  ): Unit = {
    val anyTask = if (removedTask == null) addedTask else removedTask

    editHistory.addEdit(
      documentId = 19191,
      documentEdit = DocumentEdit.Reversible(
        removedTasks = Seq() ++ Option(removedTask),
        addedTasks = Seq() ++ Option(addedTask),
        taskUpdates = Seq(),
      ),
      selectionBeforeEdit = DetachedSelection.singleton(DetachedCursor(anyTask, 0)),
      selectionAfterEdit = DetachedSelection.singleton(DetachedCursor(anyTask, 0)),
      replacementString = "",
    )
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

  private def assertEdit(
      edit: Option[Edit],
      removedTask: Task = null,
      addedTaskWithNewId: Task = null,
  ): Task = {
    if (removedTask != null) {
      assert(edit.get.documentEdit.removedTasks == Seq(removedTask))

      assert(edit.get.selectionBeforeEdit.start.taskId == removedTask.id)
    } else {
      assert(edit.get.documentEdit.removedTasks.isEmpty)
    }

    val addedTaskInEdit =
      if (addedTaskWithNewId != null) {
        val addedTaskInEdit = getOnlyElement(edit.get.documentEdit.addedTasks)
        assert(addedTaskInEdit.copyWithId(0) == addedTaskWithNewId.copyWithId(0))
        assert(addedTaskInEdit.id != addedTaskWithNewId.id)

        assert(edit.get.selectionBeforeEdit.start.taskId == addedTaskInEdit.id)
        assert(edit.get.selectionAfterEdit.start.taskId == addedTaskInEdit.id)

        addedTaskInEdit
      } else {
        assert(edit.get.documentEdit.addedTasks.isEmpty)
        null
      }

    assert(edit.get.documentEdit.taskUpdates.isEmpty)

    addedTaskInEdit
  }

  private def textWithMarkup(string: String, formatting: Formatting = Formatting.none): TextWithMarkup =
    TextWithMarkup.create(string, formatting, alreadySanitized = true)
}
