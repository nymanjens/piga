package app.flux.react.app.document

import hydro.common.GuavaReplacement.Iterables.getOnlyElement
import app.common.testing.JsTestObjects._
import app.common.testing.TestModule
import app.flux.react.app.document.EditHistory.Edit
import app.models.document.Document.DetachedCursor
import app.models.document.Document.DetachedSelection
import app.models.document.DocumentEdit
import app.models.document.Task
import utest._

import scala.collection.immutable.Seq

object EditHistoryTest extends TestSuite {

  override def tests = TestSuite {
    implicit val clock = new TestModule().fakeClock
    implicit val editHistory = new EditHistory

    "normal operation" - {
      val abc = newTask("ABC")
      val abcd = newTask("ABCD")
      val xyz = newTask("XYZ")
      val xy = newTask("XY")
      val xyx = newTask("XYX")
      val xyr = newTask("XYR")

      addSimpleEdit(abc, abcd, "D")
      addSimpleEdit(xyz, xy)
      addSimpleEdit(xy, xyx, "X")

      editHistory.redo() ==> None
      val xy_ = assertEdit(editHistory.undo(), xyx, xy)
      assertEdit(editHistory.undo(), xy_, xyz)
      val abc_ = assertEdit(editHistory.undo(), abcd, abc)
      editHistory.undo() ==> None

      val abcd_ = assertEdit(editHistory.redo(), abc_, abcd)

      addSimpleEdit(abcd_, xyr)

      editHistory.redo() ==> None
      val abcd__ = assertEdit(editHistory.undo(), xyr, abcd_)
      assertEdit(editHistory.undo(), abcd__, abc_)
      editHistory.undo() ==> None
    }
    "with mergeable edits" - {
      val abc = newTask("ABC")
      val abcd = newTask("ABCD")
      val abcdS = newTask("ABCD ")
      val abcdSe = newTask("ABCD E")
      val abcdx = newTask("ABCDX")

      "merged" - {
        addSimpleEdit(abc, abcd, "D")
        addSimpleEdit(abcd, abcdS, " ")
        addSimpleEdit(abcdS, abcdSe, "E")

        val abcd_ = assertEdit(editHistory.undo(), abcdSe, abcd)
        assertEdit(editHistory.undo(), abcd_, abc)
      }
      "not merged after undo()" - {
        addSimpleEdit(abc, abcd, "D")
        addSimpleEdit(abcd, abcdS, " ")
        addSimpleEdit(abcdS, abcdSe, "E")

        val abcd_ = assertEdit(editHistory.undo(), abcdSe, abcd)

        addSimpleEdit(abcd_, abcdx, "X")

        assertEdit(editHistory.undo(), abcdx, abcd_)
      }
    }
  }

  private def addSimpleEdit(removedTask: Task, addedTask: Task, replacementString: String = "")(
      implicit editHistory: EditHistory): Unit = {
    editHistory.addEdit(
      documentEdit = DocumentEdit.Reversible(
        removedTasks = Seq(removedTask),
        addedTasks = Seq(addedTask),
      ),
      selectionBeforeEdit = DetachedSelection.singleton(DetachedCursor(removedTask, 0)),
      selectionAfterEdit = DetachedSelection.singleton(DetachedCursor(addedTask, 0)),
      replacementString = replacementString
    )
  }
  private def assertEdit(edit: Option[Edit], removedTask: Task, addedTaskWithNewId: Task): Task = {
    assert(edit.get.documentEdit.removedTasks == Seq(removedTask))

    val addedTaskInEdit = getOnlyElement(edit.get.documentEdit.addedTasks)
    assert(addedTaskInEdit.copyWithId(0) == addedTaskWithNewId.copyWithId(0))
    assert(addedTaskInEdit.id != addedTaskWithNewId.id)

    assert(edit.get.selectionBeforeEdit.start.taskId == removedTask.id)
    assert(edit.get.selectionAfterEdit.start.taskId == addedTaskInEdit.id)

    addedTaskInEdit
  }
}
