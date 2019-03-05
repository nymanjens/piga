package app.models.document

import java.time.Duration

import hydro.common.time.JavaTimeImplicits._
import app.common.testing.JsTestObjects._
import app.common.testing.TestModule
import app.common.testing.TestObjects._
import app.models.document.DocumentEdit.MaskedTaskUpdate
import app.models.document.DocumentEdit.MaskedTaskUpdate.FieldUpdate
import hydro.common.testing.FakeClock
import hydro.models.modification.EntityModification
import utest._

import scala.collection.immutable.Seq

object DocumentEditTest extends TestSuite {

  implicit private val clock = new TestModule().fakeClock

  private val taskF = newTask("Task F")
  private val taskG = newTask("Task G")

  private val taskDUpdate = MaskedTaskUpdate
    .fromFields(originalTask = taskD, content = TextWithMarkup("edited"), orderToken = orderTokenE)
  private val reversibleEdit = DocumentEdit.Reversible(
    removedTasks = Seq(taskA, taskB),
    addedTasks = Seq(taskC, taskE),
    taskUpdates = Seq(taskDUpdate),
  )
  private val taskDAfterUpdate = taskD.withAppliedUpdateAndNewUpdateTime(taskDUpdate)

  override def tests = TestSuite {
    "DocumentEdit.Reversible" - {
      val edit = reversibleEdit
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
      val edit = DocumentEdit.WithUpdateTimes
        .create(
          removedTasksIds = Set(taskA.id, taskB.id),
          addedTasks = Seq(taskC, taskE),
          taskUpdates = Seq(taskDAfterUpdate))

      "fromReversible" - {
        implicit val document = newDocument(taskD)
        DocumentEdit.WithUpdateTimes.fromReversible(reversibleEdit) ==> edit
      }
      "mergedWith" - {
        clock.setNowInstant(FakeClock.defaultInstant.plusSeconds(60))
        val taskDAfterUpdate2 =
          taskD.withAppliedUpdateAndNewUpdateTime(
            MaskedTaskUpdate
              .fromFields(taskD, content = TextWithMarkup("edited2"), indentation = 96))
        val taskCAfterUpdate =
          taskC.withAppliedUpdateAndNewUpdateTime(MaskedTaskUpdate.fromFields(taskC, tags = Seq("xioq")))
        val otherEdit = DocumentEdit.WithUpdateTimes.create(
          removedTasksIds = Set(taskE.id, taskG.id),
          addedTasks = Seq(taskF),
          taskUpdates = Seq(taskDAfterUpdate2, taskCAfterUpdate),
        )

        val result = edit mergedWith otherEdit

        result.removedTasksIds ==> Set(taskA.id, taskB.id, taskG.id)
        result.addedTasks.toSet ==> Set(taskC, taskF)
        result.taskUpdates.toSet ==> Set(taskDAfterUpdate mergedWith taskDAfterUpdate2, taskCAfterUpdate)
      }
      "toEntityModifications" - {
        edit.toEntityModifications.toSet ==> Set(
          EntityModification.Remove[TaskEntity](taskA.id),
          EntityModification.Remove[TaskEntity](taskB.id),
          EntityModification.Add(taskC.toTaskEntity),
          EntityModification.Add(taskE.toTaskEntity),
          EntityModification.Update(taskDAfterUpdate.toTaskEntity),
        )
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
