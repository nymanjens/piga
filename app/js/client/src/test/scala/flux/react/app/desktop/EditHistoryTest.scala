package flux.react.app.desktop

import common.GuavaReplacement.Iterables
import common.GuavaReplacement.Iterables.getOnlyElement
import common.testing.TestObjects._
import common.testing.JsTestObjects._
import common.testing.TestModule
import flux.react.app.desktop.EditHistory.Edit
import models.document.Task
import scala2js.Converters._
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
      removedTasks = Seq(removedTask),
      addedTasks = Seq(addedTask),
      selectionBeforeEdit = testDetachedSelection,
      selectionAfterEdit = testDetachedSelection,
      replacementString = replacementString
    )
  }
  private def assertEdit(edit: Option[Edit], removedTask: Task, addedTaskWithNewId: Task): Task = {
    assert(edit.get.removedTasks == Seq(removedTask))

    val addedTaskInEdit = getOnlyElement(edit.get.addedTasks)
    assert(addedTaskInEdit.copyWithId(0) == addedTaskWithNewId.copyWithId(0))
    assert(addedTaskInEdit.id != addedTaskWithNewId.id)

    addedTaskInEdit
  }
}
