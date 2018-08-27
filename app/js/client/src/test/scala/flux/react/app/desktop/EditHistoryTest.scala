package flux.react.app.desktop

import common.testing.TestObjects._
import common.testing.JsTestObjects._
import common.testing.TestModule
import flux.react.app.desktop.EditHistory.Edit
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

      addSimpleEdit(abc, abcd, "D")
      addSimpleEdit(xyz, xy)
      addSimpleEdit(xy, xyx, "X")

      editHistory.redo() ==> None
      assertEdit(editHistory.undo(), xyx, xy)
      assertEdit(editHistory.undo(), xy, xyz)
      assertEdit(editHistory.undo(), abcd, abc)
      editHistory.undo() ==> None
      assertEdit(editHistory.redo(), abc, abcd)

      addSimpleEdit(abcd, xyx)

      editHistory.redo() ==> None
      assertEdit(editHistory.undo(), xyx, abcd)
      assertEdit(editHistory.undo(), abcd, abc)
      editHistory.undo() ==> None
    }
    "with mergeable edits" - {
      val abc = newTask("ABC")
      val abcd = newTask("ABCD")
      val abcd_ = newTask("ABCD ")
      val abcd_e = newTask("ABCD E")
      val abcdx = newTask("ABCDX")

      "merged" - {
        addSimpleEdit(abc, abcd, "D")
        addSimpleEdit(abcd, abcd_, " ")
        addSimpleEdit(abcd_, abcd_e, "E")

        assertEdit(editHistory.undo(), abcd_e, abcd)
        assertEdit(editHistory.undo(), abcd, abc)
      }
      "not merged after undo()" - {
        addSimpleEdit(abc, abcd, "D")
        addSimpleEdit(abcd, abcd_, " ")
        addSimpleEdit(abcd_, abcd_e, "E")

        assertEdit(editHistory.undo(), abcd_e, abcd)

        addSimpleEdit(abcd, abcdx, "X")

        assertEdit(editHistory.undo(), abcdx, abcd)
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
  private def assertEdit(edit: Option[Edit], removedTask: Task, addedTask: Task): Unit = {
    edit.get.removedTasks ==> Seq(removedTask)
    edit.get.addedTasks ==> Seq(addedTask)
  }
}
