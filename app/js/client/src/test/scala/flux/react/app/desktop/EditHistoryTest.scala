package flux.react.app.desktop

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
      // TODO
    }
    "with no-op edit" - {
      val abc1 = newTask("ABC")
      val abc2 = newTask("ABC")
      val abc3 = newTask("ABC")
      val xyz = newTask("XYZ")
      val ghi = newTask("GHI")
      val klm = newTask("KLM")

      "very first edit cannot be merged" - {
        addSimpleEdit(abc1, abc2) // = no-op
        assertEdit(editHistory.undo(), abc2, abc1)
      }

      "no-op edit is merged" - {
        "chainable edit" - {
          addSimpleEdit(xyz, abc1)
          addSimpleEdit(abc1, abc2) // = no-op

          assertEdit(editHistory.undo(), abc2, xyz)
          assertEdit(editHistory.redo(), xyz, abc2)
        }
        "unchainable edit" - {
          addSimpleEdit(xyz, ghi)
          addSimpleEdit(abc1, abc2) // = no-op

          assertEditSet(editHistory.undo(), Set(ghi, abc2), Set(xyz, abc1))
        }
        "partly chainable edit" - {
          addEdit(Seq(xyz), Seq(ghi, abc1))
          addSimpleEdit(abc1, abc2) // = no-op

          assertEditSet(editHistory.undo(), Set(ghi, abc2), Set(xyz))
        }
      }

      "branching keeps working" - {
        addSimpleEdit(xyz, abc1)
        addSimpleEdit(abc1, abc2)
        addSimpleEdit(abc2, ghi)

        assertEdit(editHistory.undo(), ghi, abc2)

        addSimpleEdit(abc2, abc3) // branch

        editHistory.redo() ==> None
        assertEdit(editHistory.undo(), klm, abc2)
        assertEdit(editHistory.undo(), abc2, xyz)
        editHistory.undo() ==> None
        assertEdit(editHistory.redo(), xyz, abc2)
        assertEdit(editHistory.redo(), abc2, klm)
        editHistory.redo() ==> None
      }
    }
  }

  private def addSimpleEdit(removedTask: Task, addedTask: Task, replacementString: String = "")(
      implicit editHistory: EditHistory): Unit = addEdit(Seq(removedTask), Seq(addedTask), replacementString)

  private def addEdit(removedTasks: Seq[Task], addedTasks: Seq[Task], replacementString: String = "")(
      implicit editHistory: EditHistory): Unit = {
    editHistory.addEdit(
      removedTasks = removedTasks,
      addedTasks = addedTasks,
      selectionBeforeEdit = testDetachedSelection,
      selectionAfterEdit = testDetachedSelection,
      replacementString = replacementString
    )
  }
  private def assertEdit(edit: Option[Edit], removedTask: Task, addedTask: Task): Unit =
    assertEditSet(edit, Set(removedTask), Set(addedTask))
  private def assertEditSet(edit: Option[Edit], removedTasks: Set[Task], addedTasks: Set[Task]): Unit = {
    edit.get.removedTasks.toSet ==> removedTasks
    edit.get.addedTasks.toSet ==> addedTasks
  }
  private def newTask(contentString: String): Task =
    Task.withRandomId(orderToken = orderTokenA, content = TextWithMarkup(contentString), indentation = 1)
}
