package app.models.document

import app.common.testing.JsTestObjects.newDocument
import app.common.testing.TestModule
import app.common.testing.TestObjects._
import app.models.access.ModelFields
import app.models.document.DocumentEdit.MaskedTaskUpdate
import hydro.models.UpdatableEntity.LastUpdateTime
import utest._

import scala.collection.immutable.Seq

object TaskTest extends TestSuite {

  implicit private val clock = new TestModule().fakeClock
  implicit private val user = new TestModule().testUser

  implicit private val document = newDocument()

  override def tests = TestSuite {

    "equalsIgnoringMetadata" - {
      val taskA1 = Task.withRandomId(
        content = TextWithMarkup("a"),
        orderToken = orderTokenA,
        indentation = 1,
        collapsed = true,
        checked = false,
        delayedUntil = Some(testDate),
        tags = Seq("a"),
      )
      val taskA2 = Task.withRandomId(
        content = TextWithMarkup("a"),
        orderToken = orderTokenA,
        indentation = 1,
        collapsed = true,
        checked = true,
        delayedUntil = Some(testDate),
        tags = Seq("a"),
      )
      val taskB = Task.withRandomId(
        content = TextWithMarkup("b"),
        orderToken = orderTokenA,
        indentation = 1,
        collapsed = true,
        checked = false,
        delayedUntil = Some(testDate),
        tags = Seq("a"),
      )

      (taskA1 equalsIgnoringMetadata taskA2) ==> true
      (taskA1 equalsIgnoringMetadata taskB) ==> false
    }

    "mergeWith" - {
      val task1 = Task.fromTaskEntity(
        TaskEntity(
          documentId = document.id,
          contentHtml = "a1",
          orderToken = orderTokenA,
          indentation = 1,
          collapsed = true,
          checked = false,
          delayedUntil = Some(testDate),
          tags = Seq("a"),
          lastContentModifierUserId = testUser.id,
          idOption = Some(123),
          lastUpdateTime =
            LastUpdateTime.someFieldsUpdated(Seq(ModelFields.TaskEntity.indentation), testInstantA),
        )
      )
      val task2 = Task.fromTaskEntity(
        TaskEntity(
          documentId = document.id,
          contentHtml = "a2",
          orderToken = orderTokenA,
          indentation = 2,
          collapsed = true,
          checked = false,
          delayedUntil = Some(testDate),
          tags = Seq("a"),
          lastContentModifierUserId = testUser.id,
          idOption = Some(123),
          lastUpdateTime =
            LastUpdateTime.someFieldsUpdated(Seq(ModelFields.TaskEntity.contentHtml), testInstantB),
        )
      )

      (task1 mergedWith task2) ==> Task.fromTaskEntity(
        TaskEntity(
          documentId = document.id,
          contentHtml = "a2",
          orderToken = orderTokenA,
          indentation = 1,
          collapsed = true,
          checked = true,
          delayedUntil = Some(testDate),
          tags = Seq("a"),
          lastContentModifierUserId = testUser.id,
          idOption = Some(123),
          lastUpdateTime = LastUpdateTime(
            timePerField = Map(
              ModelFields.TaskEntity.indentation -> testInstantA,
              ModelFields.TaskEntity.contentHtml -> testInstantB,
            ),
            otherFieldsTime = None,
          ),
        )
      )
    }
    "withAppliedUpdateAndNewUpdateTime" - {
      val task = Task.fromTaskEntity(
        TaskEntity(
          documentId = document.id,
          contentHtml = "A",
          orderToken = orderTokenA,
          indentation = 1,
          collapsed = true,
          checked = true,
          delayedUntil = Some(testDate),
          tags = Seq("a"),
          lastContentModifierUserId = testUser.id,
          idOption = Some(123),
          lastUpdateTime =
            LastUpdateTime.someFieldsUpdated(Seq(ModelFields.TaskEntity.indentation), testInstantA),
        )
      )

      val taskAfterUpdate = Task.fromTaskEntity(
        TaskEntity(
          documentId = document.id,
          contentHtml = "B",
          orderToken = orderTokenA,
          indentation = 1,
          collapsed = true,
          checked = true,
          delayedUntil = Some(testDate),
          tags = Seq("a"),
          lastContentModifierUserId = testUser.id,
          idOption = Some(123),
          lastUpdateTime = LastUpdateTime(
            timePerField = Map(
              ModelFields.TaskEntity.indentation -> testInstantA,
              ModelFields.TaskEntity.contentHtml -> testInstantB,
            ),
            otherFieldsTime = None,
          ),
        )
      )

      "with matching baseline" - {
        clock.setNowInstant(testInstantB)
        task.withAppliedUpdateAndNewUpdateTime(
          MaskedTaskUpdate.fromFields(originalTask = task, content = TextWithMarkup("B"))
        ) ==>
          taskAfterUpdate
      }
      "with different baseline" - {
        clock.setNowInstant(testInstantC)
        task.withAppliedUpdateAndNewUpdateTime(
          MaskedTaskUpdate.fromFields(originalTask = taskAfterUpdate, tags = Seq("B"))
        ) ==>
          Task.fromTaskEntity(
            TaskEntity(
              documentId = document.id,
              contentHtml = "A",
              orderToken = orderTokenA,
              indentation = 1,
              collapsed = true,
              checked = true,
              delayedUntil = Some(testDate),
              tags = Seq("B"),
              lastContentModifierUserId = testUser.id,
              idOption = Some(123),
              lastUpdateTime = LastUpdateTime(
                timePerField = Map(
                  ModelFields.TaskEntity.indentation -> testInstantA,
                  ModelFields.TaskEntity.tags -> testInstantC,
                ),
                otherFieldsTime = None,
              ),
            )
          )
      }
    }

    "equals and hashCode" - {
      val task1A = Task.withRandomId(
        content = TextWithMarkup("a"),
        orderToken = orderTokenA,
        indentation = 1,
        collapsed = true,
        checked = false,
        delayedUntil = Some(testDate),
        tags = Seq("a"),
      )
      val task1B = Task.fromTaskEntity(
        TaskEntity(
          documentId = document.id,
          contentHtml = "a",
          orderToken = orderTokenA,
          indentation = 1,
          collapsed = true,
          checked = false,
          delayedUntil = Some(testDate),
          tags = Seq("a"),
          lastContentModifierUserId = testUser.id,
          idOption = Some(task1A.id),
        )
      )
      val task1WithNewIndentation = Task.fromTaskEntity(
        TaskEntity(
          documentId = document.id,
          contentHtml = "a",
          orderToken = orderTokenA,
          indentation = 2,
          collapsed = true,
          checked = false,
          delayedUntil = Some(testDate),
          tags = Seq("a"),
          lastContentModifierUserId = testUser.id,
          idOption = Some(task1A.id),
        )
      )
      val task2 = Task.withRandomId(
        content = TextWithMarkup("a"),
        orderToken = orderTokenA,
        indentation = 1,
        collapsed = true,
        checked = true,
        delayedUntil = Some(testDate),
        tags = Seq("a"),
      )

      task1A == task1B ==> true
      task1A == task1WithNewIndentation ==> false
      task1A == task2 ==> false

      task1A.hashCode == task1B.hashCode ==> true
      task1A.hashCode == task1WithNewIndentation.hashCode ==> false
      task1A.hashCode == task2.hashCode ==> false
    }
  }
}
