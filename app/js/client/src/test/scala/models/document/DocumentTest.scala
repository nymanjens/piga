package models.document

import common.OrderToken
import common.testing.JsTestObjects._
import common.testing.TestObjects._
import models.document.Document.{DetachedCursor, DetachedSelection, IndexedCursor, IndexedSelection}
import scala2js.Converters._
import utest._

import scala.collection.immutable.Seq

object DocumentTest extends TestSuite {
  val taskBB = newTask("Task BB", orderToken = orderTokenB)
  val taskEE = newTask("Task EE", orderToken = orderTokenE)
  val taskF = newTask("Task F", orderToken = OrderToken.middleBetween(Some(orderTokenE), None))

  override def tests = TestSuite {
    "equals and hashCode" - {
      val documentA: AnyRef =
        new Document(id = 12873, name = "test document", orderTokenA, tasks = Seq(taskA))
      val documentA2: AnyRef =
        new Document(id = 12873, name = "test document", orderTokenA, tasks = Seq(taskA))
      val documentB: AnyRef =
        new Document(id = 12873, name = "test document", orderTokenA, tasks = Seq(taskB))
      val documentC: AnyRef =
        new Document(id = 12873, name = "CCCCCCCCCCCCC", orderTokenA, tasks = Seq(taskA))
      val documentD: AnyRef =
        new Document(id = 11111, name = "test document", orderTokenA, tasks = Seq(taskA))
      val documentE: AnyRef =
        new Document(id = 12873, name = "test document", orderTokenB, tasks = Seq(taskA))

      documentA.hashCode() == documentA.asInstanceOf[Document].hashCode ==> true
      documentA == documentA2 ==> true
      documentA.hashCode() == documentA2.hashCode() ==> true

      documentA == documentB ==> false
      documentA.hashCode() == documentB.hashCode() ==> false

      documentA == documentC ==> false
      documentA.hashCode() == documentC.hashCode() ==> false

      documentA == documentD ==> false
      documentA.hashCode() == documentD.hashCode() ==> false

      documentA == documentE ==> false
      documentA.hashCode() == documentE.hashCode() ==> false
    }
    "replaced" - {
      val document = newDocument(taskA, taskB, taskC)

      document.replaced(toReplace = Seq(taskB), toAdd = Seq(taskBB)) ==>
        newDocument(taskA, taskBB, taskC)
      document.replaced(toReplace = Seq(taskB, taskC), toAdd = Seq(taskD, taskE)) ==>
        newDocument(taskA, taskD, taskE)
      document.replaced(toReplace = Seq(taskA), toAdd = Seq(taskD)) ==>
        newDocument(taskB, taskC, taskD)
      document.replaced(toReplace = Seq(taskA), toAdd = Seq(taskC, taskD, taskE)) ==>
        newDocument(taskB, taskC, taskD, taskE)
      newDocument(taskB, taskD, taskE).replaced(toReplace = Seq(taskB), toAdd = Seq(taskC, taskE)) ==>
        newDocument(taskC, taskD, taskE)
    }
    "plus" - {
      val document = newDocument(taskB, taskD)
      "first task" - {
        document + taskA ==> newDocument(taskA, taskB, taskD)
      }
      "last task" - {
        document + taskE ==> newDocument(taskB, taskD, taskE)
      }
      "middle task" - {
        document + taskC ==> newDocument(taskB, taskC, taskD)
      }
      "task is already in list" - {
        document + taskB ==> document
      }
    }
    "minusTaskWithId" - {
      val document = newDocument(taskB, taskD)
      "existing task" - {
        document.minusTaskWithId(taskB.id) ==> newDocument(taskD)
      }
      "task not in list" - {
        document.minusTaskWithId(18230983210L) ==> newDocument(taskB, taskD)
      }
    }
    "indexOf" - {
      val document = newDocument(taskA, taskB, taskBB, taskC, taskD, taskE, taskEE)

      document.indexOf(taskA) ==> 0
      document.indexOf(taskB) ==> 1
      document.indexOf(taskBB) ==> 2
      document.indexOf(taskC) ==> 3
      document.indexOf(taskD) ==> 4
      document.indexOf(taskE) ==> 5
      document.indexOf(taskEE) ==> 6
    }
    "collapsedTasksRange" - {
      "none collapsed" - {
        val document = newDocument(taskA, taskB, taskC, taskD, taskE)

        document.collapsedTasksRange(0) ==> None
        document.collapsedTasksRange(1) ==> None
        document.collapsedTasksRange(2) ==> None
        document.collapsedTasksRange(3) ==> None
        document.collapsedTasksRange(4) ==> None
      }
      "collapsed without children" - {
        val document = newDocument(
          indentation(0, taskA),
          indentation(1, taskB),
          indentation(2, collapsed(taskC)),
          indentation(1, taskD),
          indentation(0, taskE))

        document.collapsedTasksRange(0) ==> None
        document.collapsedTasksRange(1) ==> None
        document.collapsedTasksRange(2) ==> Some(document.CollapsedTasksRange(2, 2))
        document.collapsedTasksRange(3) ==> None
        document.collapsedTasksRange(4) ==> None
      }
      "collapsed with children" - {
        val document = newDocument(
          indentation(1, taskA),
          indentation(1, collapsed(taskB)),
          indentation(2, taskC),
          indentation(2, taskD),
          indentation(1, taskE),
          indentation(2, taskF))

        document.collapsedTasksRange(0) ==> None
        document.collapsedTasksRange(1) ==> Some(document.CollapsedTasksRange(1, 3))
        document.collapsedTasksRange(2) ==> Some(document.CollapsedTasksRange(1, 3))
        document.collapsedTasksRange(3) ==> Some(document.CollapsedTasksRange(1, 3))
        document.collapsedTasksRange(4) ==> None
        document.collapsedTasksRange(5) ==> None
      }
      "collapsed with bump" - {
        val document = newDocument(
          indentation(0, collapsed(taskA)),
          indentation(2, taskB),
          indentation(1, taskC),
          indentation(0, taskD),
          indentation(1, taskE))

        document.collapsedTasksRange(0) ==> Some(document.CollapsedTasksRange(0, 2))
        document.collapsedTasksRange(1) ==> Some(document.CollapsedTasksRange(0, 2))
        document.collapsedTasksRange(2) ==> Some(document.CollapsedTasksRange(0, 2))
        document.collapsedTasksRange(3) ==> None
        document.collapsedTasksRange(4) ==> None
      }
      "all collapsed" - {
        val document = newDocument(
          indentation(1, collapsed(taskA)),
          indentation(1, collapsed(taskB)),
          indentation(2, collapsed(taskC)),
          indentation(3, collapsed(taskD)),
          indentation(1, collapsed(taskE))
        )

        document.collapsedTasksRange(0) ==> Some(document.CollapsedTasksRange(0, 0))
        document.collapsedTasksRange(1) ==> Some(document.CollapsedTasksRange(1, 3))
        document.collapsedTasksRange(2) ==> Some(document.CollapsedTasksRange(2, 3))
        document.collapsedTasksRange(3) ==> Some(document.CollapsedTasksRange(3, 3))
        document.collapsedTasksRange(4) ==> Some(document.CollapsedTasksRange(4, 4))
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
        def assertNoChange(collapsedOnly: Boolean,
                           selection: IndexedSelection)(implicit document: Document): Unit = {
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
            indentation(0, taskE))

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
            indentation(0, taskE))

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
            indentation(1, taskE))

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

  private def collapsed(task: Task): Task = task.copyWithRandomId(collapsed = true)
  private def indentation(newIndentation: Int, task: Task): Task =
    task.copyWithRandomId(indentation = newIndentation)
}
