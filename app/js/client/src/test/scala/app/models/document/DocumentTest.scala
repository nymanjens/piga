package app.models.document

import app.common.testing.JsTestObjects._
import app.common.testing.TestModule
import app.common.testing.TestObjects._
import app.models.document.Document.DetachedCursor
import app.models.document.Document.DetachedSelection
import app.models.document.Document.IndexedCursor
import app.models.document.Document.IndexedSelection
import app.models.document.DocumentEdit.MaskedTaskUpdate
import app.models.document.TextWithMarkup.Formatting
import hydro.common.OrderToken
import utest._

import scala.collection.immutable.Seq

object DocumentTest extends TestSuite {

  implicit private val fakeClock = new TestModule().fakeClock
  implicit private val testUser = new TestModule().testUser

  val taskBB = newTask("Task BB", orderToken = orderTokenB)
  val taskEE = newTask("Task EE", orderToken = orderTokenE)
  val taskF = newTask("Task F", orderToken = OrderToken.middleBetween(Some(orderTokenE), None))

  override def tests = TestSuite {
    "withAppliedEdit" - {
      "tasks removed" - {
        val document = newDocument(taskA, taskB, taskC)
        val edit = DocumentEdit.WithUpdateTimes
          .create(removedTasksIds = Seq(taskB.id), addedTasks = Seq(), taskUpdates = Seq())

        "removes" - {
          document.withAppliedEdit(edit) ==> newDocument(taskA, taskC)
        }
        "is idempotent" - {
          document.withAppliedEdit(edit).withAppliedEdit(edit) ==> document.withAppliedEdit(edit)
        }
      }
      "tasks added" - {
        val document = newDocument(taskA, taskB, taskD)
        val edit = DocumentEdit.WithUpdateTimes
          .create(removedTasksIds = Seq(), addedTasks = Seq(taskC), taskUpdates = Seq())

        "adds" - {
          document.withAppliedEdit(edit) ==> newDocument(taskA, taskB, taskC, taskD)
        }
        "is idempotent" - {
          document.withAppliedEdit(edit).withAppliedEdit(edit) ==> document.withAppliedEdit(edit)
        }
      }
      "tasks updated" - {
        val document = newDocument(taskA, taskB, taskC)

        "single update" - {
          val taskBAfterUpdate = taskB.withAppliedUpdateAndNewUpdateTime(
            MaskedTaskUpdate.fromFields(originalTask = taskB, content = textWithMarkup("edited"))
          )
          val edit = DocumentEdit.WithUpdateTimes
            .create(removedTasksIds = Seq(), addedTasks = Seq(), taskUpdates = Seq(taskBAfterUpdate))

          document.withAppliedEdit(edit) ==> newDocument(taskA, taskBAfterUpdate, taskC)
        }
        "single update with changing orderToken" - {
          val taskBAfterUpdate = taskB.withAppliedUpdateAndNewUpdateTime(
            MaskedTaskUpdate
              .fromFields(originalTask = taskB, content = textWithMarkup("edited"), orderToken = orderTokenE)
          )
          val edit = DocumentEdit.WithUpdateTimes
            .create(removedTasksIds = Seq(), addedTasks = Seq(), taskUpdates = Seq(taskBAfterUpdate))

          document.withAppliedEdit(edit) ==> newDocument(taskA, taskC, taskBAfterUpdate)
        }
        "multiple updates" - {
          val taskAAfterUpdate = taskA.withAppliedUpdateAndNewUpdateTime(
            MaskedTaskUpdate.fromFields(originalTask = taskA, content = textWithMarkup("edited A"))
          )
          val taskBAfterUpdate = taskB.withAppliedUpdateAndNewUpdateTime(
            MaskedTaskUpdate.fromFields(originalTask = taskB, content = textWithMarkup("edited B"))
          )
          val edit = DocumentEdit.WithUpdateTimes
            .create(
              removedTasksIds = Seq(),
              addedTasks = Seq(),
              taskUpdates = Seq(taskAAfterUpdate, taskBAfterUpdate),
            )

          document.withAppliedEdit(edit) ==> newDocument(taskAAfterUpdate, taskBAfterUpdate, taskC)
        }
        "is idempotent" - {
          val taskBAfterUpdate = taskB.withAppliedUpdateAndNewUpdateTime(
            MaskedTaskUpdate.fromFields(originalTask = taskB, content = textWithMarkup("edited"))
          )
          val edit = DocumentEdit.WithUpdateTimes
            .create(removedTasksIds = Seq(), addedTasks = Seq(), taskUpdates = Seq(taskBAfterUpdate))

          document.withAppliedEdit(edit).withAppliedEdit(edit) ==> document.withAppliedEdit(edit)
        }
      }
      "add and update in same edit" - {
        val document = newDocument(taskA, taskC, taskD)
        val taskBAfterUpdate = taskB.withAppliedUpdateAndNewUpdateTime(
          MaskedTaskUpdate.fromFields(
            originalTask = taskB,
            orderToken = OrderToken.middleBetween(Some(taskC.orderToken), Some(taskD.orderToken)),
          )
        )
        val edit = DocumentEdit.WithUpdateTimes
          .create(removedTasksIds = Seq(), addedTasks = Seq(taskB), taskUpdates = Seq(taskBAfterUpdate))

        document.withAppliedEdit(edit) ==> newDocument(taskA, taskC, taskBAfterUpdate, taskD)
      }
      "add already existing task" - {
        val document = newDocument(taskA, taskB, taskC)
        val edit = DocumentEdit.WithUpdateTimes
          .create(
            removedTasksIds = Seq(),
            addedTasks =
              Seq(taskB.copyForTests(orderToken = OrderToken.middleBetween(None, Some(taskA.orderToken)))),
            taskUpdates = Seq(),
          )

        document.withAppliedEdit(edit) ==> newDocument(taskA, taskB, taskC)
      }
      "addition and removal" - {
        val document = newDocument(taskA, taskB, taskC)
        val edit = DocumentEdit.WithUpdateTimes
          .create(removedTasksIds = Seq(taskD.id), addedTasks = Seq(taskD), taskUpdates = Seq())

        document.withAppliedEdit(edit) ==> newDocument(taskA, taskB, taskC)
      }
    }
    "equals and hashCode" - {
      val documentA: AnyRef =
        new Document(id = 12873, name = "test document", tasks = Seq(taskA))
      val documentA2: AnyRef =
        new Document(id = 12873, name = "test document", tasks = Seq(taskA))
      val documentB: AnyRef =
        new Document(id = 12873, name = "test document", tasks = Seq(taskB))
      val documentC: AnyRef =
        new Document(id = 12873, name = "CCCCCCCCCCCCC", tasks = Seq(taskA))
      val documentD: AnyRef =
        new Document(id = 11111, name = "test document", tasks = Seq(taskA))

      documentA.hashCode() == documentA.asInstanceOf[Document].hashCode ==> true
      documentA == documentA2 ==> true
      documentA.hashCode() == documentA2.hashCode() ==> true

      documentA == documentB ==> false
      documentA.hashCode() == documentB.hashCode() ==> false

      documentA == documentC ==> false
      documentA.hashCode() == documentC.hashCode() ==> false

      documentA == documentD ==> false
      documentA.hashCode() == documentD.hashCode() ==> false
    }
    "maybeIndexOf" - {
      val document = newDocument(taskA, taskB, taskBB, taskC, taskD, taskE, taskEE)
      val orderTokenHint = OrderToken.middle

      document.maybeIndexOf(taskA.id, orderTokenHint) ==> Some(0)
      document.maybeIndexOf(taskB.id, orderTokenHint) ==> Some(1)
      document.maybeIndexOf(taskBB.id, orderTokenHint) ==> Some(2)
      document.maybeIndexOf(taskC.id, orderTokenHint) ==> Some(3)
      document.maybeIndexOf(taskD.id, orderTokenHint) ==> Some(4)
      document.maybeIndexOf(taskE.id, orderTokenHint) ==> Some(5)
      document.maybeIndexOf(taskEE.id, orderTokenHint) ==> Some(6)
      document.maybeIndexOf(taskF.id, orderTokenHint) ==> None
    }
    "visibleTaskOption" - {
      "Flat list" - {
        val document = newDocument(
          indentation(1, collapsed(taskA)),
          indentation(1, taskB),
          indentation(1, collapsed(taskC)),
          indentation(1, taskD),
          indentation(1, collapsed(taskE)),
        )

        for (minExpandedIndentation <- -1 to 3) {
          document.visibleTaskOption(0, minExpandedIndentation) ==> Some(document.tasks(0))
          document.visibleTaskOption(1, minExpandedIndentation) ==> Some(document.tasks(1))
          document.visibleTaskOption(2, minExpandedIndentation) ==> Some(document.tasks(2))
          document.visibleTaskOption(3, minExpandedIndentation) ==> Some(document.tasks(3))
          document.visibleTaskOption(4, minExpandedIndentation) ==> Some(document.tasks(4))
        }
      }
      "Tree without bumps" - {
        val document = newDocument(
          indentation(0, taskA),
          indentation(1, collapsed(taskB)),
          indentation(2, taskC),
          indentation(3, taskD),
          indentation(0, collapsed(taskE)),
        )

        for (minExpandedIndentation <- -1 to 3) {
          document.visibleTaskOption(0, minExpandedIndentation) ==> Some(document.tasks(0))
          document.visibleTaskOption(1, minExpandedIndentation) ==> Some(document.tasks(1))
          document.visibleTaskOption(4, minExpandedIndentation) ==> Some(document.tasks(4))
        }
        for (minExpandedIndentation <- -1 to 0) {
          document.visibleTaskOption(2, minExpandedIndentation) ==> None
          document.visibleTaskOption(3, minExpandedIndentation) ==> None
        }
      }
      "Tree with bump" - {
        val document = newDocument(
          indentation(0, collapsed(taskA)),
          indentation(2, taskB),
          indentation(1, taskC),
          indentation(0, taskD),
          indentation(1, taskE),
        )

        for (minExpandedIndentation <- -1 to 3) {
          document.visibleTaskOption(0, minExpandedIndentation) ==> Some(document.tasks(0))
          document.visibleTaskOption(3, minExpandedIndentation) ==> Some(document.tasks(3))
          document.visibleTaskOption(4, minExpandedIndentation) ==> Some(document.tasks(4))
        }
        document.visibleTaskOption(1, minExpandedIndentation = -1) ==> None
        document.visibleTaskOption(2, minExpandedIndentation = -1) ==> None
      }
    }
    "familyTreeRange" - {
      "Flat list" - {
        val document = newDocument(
          indentation(1, taskA),
          indentation(1, taskB),
          indentation(1, taskC),
          indentation(1, taskD),
          indentation(1, taskE),
        )

        document.familyTreeRange(0, rootParentIndentation = 0) ==> None
        document.familyTreeRange(1, rootParentIndentation = 0) ==> None
        document.familyTreeRange(2, rootParentIndentation = 0) ==> None
        document.familyTreeRange(3, rootParentIndentation = 0) ==> None
        document.familyTreeRange(4, rootParentIndentation = 0) ==> None

        document.familyTreeRange(0, rootParentIndentation = 1) ==> Some(document.FamilyTreeRange(0, 0))
        document.familyTreeRange(1, rootParentIndentation = 1) ==> Some(document.FamilyTreeRange(1, 1))
        document.familyTreeRange(2, rootParentIndentation = 1) ==> Some(document.FamilyTreeRange(2, 2))
        document.familyTreeRange(3, rootParentIndentation = 1) ==> Some(document.FamilyTreeRange(3, 3))
        document.familyTreeRange(4, rootParentIndentation = 1) ==> Some(document.FamilyTreeRange(4, 4))

        document.familyTreeRange(0, rootParentIndentation = 2) ==> None
        document.familyTreeRange(1, rootParentIndentation = 2) ==> None
        document.familyTreeRange(2, rootParentIndentation = 2) ==> None
        document.familyTreeRange(3, rootParentIndentation = 2) ==> None
        document.familyTreeRange(4, rootParentIndentation = 2) ==> None
      }
      "Tree without bumps" - {
        val document = newDocument(
          indentation(0, taskA),
          indentation(1, taskB),
          indentation(2, collapsed(taskC)),
          indentation(1, taskD),
          indentation(0, taskE),
        )

        document.familyTreeRange(0, rootParentIndentation = 0) ==> Some(document.FamilyTreeRange(0, 3))
        document.familyTreeRange(1, rootParentIndentation = 0) ==> Some(document.FamilyTreeRange(0, 3))
        document.familyTreeRange(2, rootParentIndentation = 0) ==> Some(document.FamilyTreeRange(0, 3))
        document.familyTreeRange(3, rootParentIndentation = 0) ==> Some(document.FamilyTreeRange(0, 3))
        document.familyTreeRange(4, rootParentIndentation = 0) ==> Some(document.FamilyTreeRange(4, 4))

        document.familyTreeRange(0, rootParentIndentation = 1) ==> None
        document.familyTreeRange(1, rootParentIndentation = 1) ==> Some(document.FamilyTreeRange(1, 2))
        document.familyTreeRange(2, rootParentIndentation = 1) ==> Some(document.FamilyTreeRange(1, 2))
        document.familyTreeRange(3, rootParentIndentation = 1) ==> Some(document.FamilyTreeRange(3, 3))
        document.familyTreeRange(4, rootParentIndentation = 1) ==> None

        document.familyTreeRange(0, rootParentIndentation = 2) ==> None
        document.familyTreeRange(1, rootParentIndentation = 2) ==> None
        document.familyTreeRange(2, rootParentIndentation = 2) ==> Some(document.FamilyTreeRange(2, 2))
        document.familyTreeRange(3, rootParentIndentation = 2) ==> None
        document.familyTreeRange(4, rootParentIndentation = 2) ==> None
      }
      "Tree with bump" - {
        val document = newDocument(
          indentation(0, collapsed(taskA)),
          indentation(2, taskB),
          indentation(1, taskC),
          indentation(0, taskD),
          indentation(1, taskE),
        )

        document.familyTreeRange(0, rootParentIndentation = 0) ==> Some(document.FamilyTreeRange(0, 2))
        document.familyTreeRange(1, rootParentIndentation = 0) ==> Some(document.FamilyTreeRange(0, 2))
        document.familyTreeRange(2, rootParentIndentation = 0) ==> Some(document.FamilyTreeRange(0, 2))
        document.familyTreeRange(3, rootParentIndentation = 0) ==> Some(document.FamilyTreeRange(3, 4))
        document.familyTreeRange(4, rootParentIndentation = 0) ==> Some(document.FamilyTreeRange(3, 4))

        document.familyTreeRange(0, rootParentIndentation = 1) ==> None
        document.familyTreeRange(1, rootParentIndentation = 1) ==> None
        document.familyTreeRange(2, rootParentIndentation = 1) ==> Some(document.FamilyTreeRange(2, 2))
        document.familyTreeRange(3, rootParentIndentation = 1) ==> None
        document.familyTreeRange(4, rootParentIndentation = 1) ==> Some(document.FamilyTreeRange(4, 4))

        document.familyTreeRange(0, rootParentIndentation = 2) ==> None
        document.familyTreeRange(1, rootParentIndentation = 2) ==> Some(document.FamilyTreeRange(1, 1))
        document.familyTreeRange(2, rootParentIndentation = 2) ==> None
        document.familyTreeRange(3, rootParentIndentation = 2) ==> None
        document.familyTreeRange(4, rootParentIndentation = 2) ==> None
      }
    }
    "IndexedCursor" - {
      "plusWord" - {
        implicit val document = newDocument(newTask("the red apple"))
        IndexedCursor(0, 0).plusWord ==> IndexedCursor(0, 3)
        IndexedCursor(0, 3).plusWord ==> IndexedCursor(0, 7)
        IndexedCursor(0, 4).plusWord ==> IndexedCursor(0, 7)
        IndexedCursor(0, 7).plusWord ==> IndexedCursor(0, 13)
        IndexedCursor(0, 13).plusWord ==> IndexedCursor(0, 13)
      }
    }
    "IndexedSelection" - {
      "detach" - {
        val task1 = newTask("the red apple")
        val task2 = newTask("the blue apple")
        implicit val document = newDocument(task1, task2)
        val selection = IndexedSelection(IndexedCursor(0, 0), IndexedCursor(1, 5))

        selection.detach ==>
          DetachedSelection(DetachedCursor(task1, 0), DetachedCursor(task2, 5))
        selection.detach.attachToDocument ==> selection
      }
      "includeChildren" - {
        def assertNoChange(
            collapsedOnly: Boolean,
            selection: IndexedSelection,
        )(implicit document: Document): Unit = {
          selection.includeChildren(collapsedOnly = collapsedOnly) ==> selection
        }

        "none collapsed" - {
          implicit val document = newDocument(taskA, taskB, taskC, taskD, taskE)

          for (collapsedOnly <- Seq(false, true)) {
            assertNoChange(collapsedOnly, IndexedSelection(IndexedCursor(0, 1), IndexedCursor(2, 5)))
            assertNoChange(collapsedOnly, IndexedSelection.singleton(IndexedCursor(2, 5)))
          }
        }
        "(collapsedOnly = false) non-collapsed tree" - {
          implicit val document = newDocument(
            indentation(0, taskA),
            indentation(1, taskB),
            indentation(2, collapsed(taskC)),
            indentation(1, taskD),
            indentation(0, taskE),
          )

          assertNoChange(collapsedOnly = false, IndexedSelection(IndexedCursor(0, 1), IndexedCursor(2, 5)))
          assertNoChange(collapsedOnly = false, IndexedSelection.singleton(IndexedCursor(2, 5)))
          IndexedSelection(IndexedCursor(0, 1), IndexedCursor(1, 5))
            .includeChildren(collapsedOnly = false) ==>
            IndexedSelection(IndexedCursor(0, 1), IndexedCursor(2, 0))
          IndexedSelection.singleton(IndexedCursor(1, 5)).includeChildren(collapsedOnly = false) ==>
            IndexedSelection(IndexedCursor(1, 5), IndexedCursor(2, 0))
        }
        "(collapsedOnly = true) collapsed without children" - {
          implicit val document = newDocument(
            indentation(0, taskA),
            indentation(1, taskB),
            indentation(2, collapsed(taskC)),
            indentation(1, taskD),
            indentation(0, taskE),
          )

          assertNoChange(collapsedOnly = true, IndexedSelection.singleton(IndexedCursor(0, 0)))
          assertNoChange(collapsedOnly = true, IndexedSelection.singleton(IndexedCursor(1, 0)))
          assertNoChange(collapsedOnly = true, IndexedSelection(IndexedCursor(0, 1), IndexedCursor(2, 5)))
          assertNoChange(collapsedOnly = true, IndexedSelection.singleton(IndexedCursor(2, 5)))
        }
        "collapsed with children" - {
          implicit val document = newDocument(
            indentation(1, taskA),
            indentation(1, collapsed(taskB)),
            indentation(2, taskC),
            indentation(2, taskD),
            indentation(1, taskE),
          )

          for (collapsedOnly <- Seq(false, true)) {
            IndexedSelection(IndexedCursor(0, 1), IndexedCursor(1, 5))
              .includeChildren(collapsedOnly = collapsedOnly) ==>
              IndexedSelection(IndexedCursor(0, 1), IndexedCursor(3, 0))
            IndexedSelection.singleton(IndexedCursor(1, 5)).includeChildren(collapsedOnly = collapsedOnly) ==>
              IndexedSelection(IndexedCursor(1, 5), IndexedCursor(3, 0))
          }
        }
      }
    }
  }

  private def collapsed(task: Task): Task = task.copyForTests(collapsed = true)
  private def indentation(newIndentation: Int, task: Task): Task =
    task.copyForTests(indentation = newIndentation)
}
