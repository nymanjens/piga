package app.models.document

import app.common.testing.JsTestObjects._
import app.common.testing.TestObjects._
import app.models.document.DocumentEdit.MaskedTaskUpdate
import app.models.document.DocumentEdit.MaskedTaskUpdate.FieldUpdate
import utest._

import scala.collection.immutable.Seq

object DocumentEditTest extends TestSuite {

  val taskF = newTask("Task F")
  val taskG = newTask("Task G")

  override def tests = TestSuite {
    "DocumentEdit.Reversible" - {
      val edit = DocumentEdit.Reversible(
        removedTasks = Seq(taskA, taskB),
        addedTasks = Seq(taskC, taskE),
        taskUpdates = Seq(
          MaskedTaskUpdate
            .fromFields(originalTask = taskD, content = TextWithMarkup("edited"), orderToken = orderTokenE)),
      )
      val taskDAfterUpdate = taskD.copyForTests(content = TextWithMarkup("edited"), orderToken = orderTokenE)
      "reversed" - {
        edit.reversed ==> DocumentEdit.Reversible(
          removedTasks = Seq(taskC, taskE),
          addedTasks = Seq(taskA, taskB),
          taskUpdates = Seq(
            MaskedTaskUpdate
              .fromFields(
                originalTask = taskDAfterUpdate,
                content = taskD.content,
                orderToken = taskD.orderToken)),
        )
      }
      "mergedWith" - {
        val otherEdit = DocumentEdit.Reversible(
          removedTasks = Seq(taskE, taskG),
          addedTasks = Seq(taskF),
          taskUpdates = Seq(
            MaskedTaskUpdate.fromFields(
              originalTask = taskDAfterUpdate,
              content = TextWithMarkup("edited2"),
              indentation = 96),
            MaskedTaskUpdate.fromFields(originalTask = taskC, tags = Seq("xioq")),
          ),
        )

        val result = edit mergedWith otherEdit

        result.removedTasks.toSet ==> Set(taskA, taskB, taskG)
        result.addedTasks.toSet ==> Set(taskC, taskF)
        result.taskUpdates.toSet ==> Set(
          MaskedTaskUpdate.fromFields(
            originalTask = taskD,
            content = TextWithMarkup("edited2"),
            orderToken = orderTokenE,
            indentation = 96),
          MaskedTaskUpdate.fromFields(originalTask = taskC, tags = Seq("xioq")),
        )
      }
      "isNoOp" - {
        def assertNotANoOp(edit: DocumentEdit.Reversible): Unit = assert(!edit.isNoOp)
        def assertNoOp(edit: DocumentEdit.Reversible): Unit = assert(edit.isNoOp)

        "Not a no-op" - {
          assertNotANoOp(edit)
          assertNotANoOp(DocumentEdit.Reversible(removedTasks = Seq(taskB), addedTasks = Seq(taskA, taskB)))
          assertNotANoOp(
            DocumentEdit.Reversible(removedTasks = Seq(taskA, taskB), addedTasks = Seq(taskB, taskC)))
        }
        "Is no-op" - {
          "empty edit" - {
            assertNoOp(DocumentEdit.Reversible())
          }
          "removed == added" - {
            assertNoOp(
              DocumentEdit.Reversible(removedTasks = Seq(taskA, taskB), addedTasks = Seq(taskA, taskB)))
          }
          "empty updates" - {
            assertNoOp(
              DocumentEdit.Reversible(
                taskUpdates = Seq(MaskedTaskUpdate.fromFields(taskA, content = taskA.content))))
            assertNoOp(
              DocumentEdit.Reversible(
                taskUpdates = Seq(MaskedTaskUpdate.fromFields(taskA, indentation = taskA.indentation))))
          }
        }
      }
    }
    "DocumentEdit.WithUpdateTimes" - {
      "fromReversible" - {
        1 ==> 0
      }
      "mergedWith" - {
        1 ==> 0
      }
      "toEntityModifications" - {
        1 ==> 0
      }
    }
    "MaskedTaskUpdate" - {
      "fromFields" - {
        val update = MaskedTaskUpdate
          .fromFields(originalTask = testTask, content = TextWithMarkup("edited"), orderToken = orderTokenE)

        update ==> MaskedTaskUpdate(
          taskId = testTask.id,
          originalOrderToken = testTask.orderToken,
          content = Some(FieldUpdate(testTask.content, TextWithMarkup("edited"))),
          orderToken = Some(FieldUpdate(testTask.orderToken, orderTokenE)),
          indentation = None,
          collapsed = None,
          delayedUntil = None,
          tags = None,
        )
      }
    }
  }
}
