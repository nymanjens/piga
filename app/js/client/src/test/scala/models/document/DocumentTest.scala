package models.document

import common.testing.JsTestObjects._
import common.testing.TestObjects._
import models.document.Document.{DetachedCursor, DetachedSelection, IndexedCursor, IndexedSelection}
import scala2js.Converters._
import utest._

import scala.collection.immutable.Seq

object DocumentTest extends TestSuite {
  val taskBB = newTask("Task BB", orderToken = orderTokenB)
  val taskEE = newTask("Task EE", orderToken = orderTokenE)

  override def tests = TestSuite {
    "equals and hashCode" - {
      val documentA: AnyRef = new Document(id = 12873, name = "test document", tasks = Seq(taskA))
      val documentA2: AnyRef = new Document(id = 12873, name = "test document", tasks = Seq(taskA))
      val documentB: AnyRef = new Document(id = 12873, name = "test document", tasks = Seq(taskB))
      val documentC: AnyRef = new Document(id = 12873, name = "CCCCCCCCCCCCC", tasks = Seq(taskA))
      val documentD: AnyRef = new Document(id = 11111, name = "test document", tasks = Seq(taskA))

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
    "replaced" - {
      val document = newDocument(taskA, taskB, taskC)

      document.replaced(toReplace = Seq(taskB), toAdd = Seq(taskBB)) ==>
        newDocument(taskA, taskBB, taskC)
      document.replaced(toReplace = Seq(taskB, taskC), toAdd = Seq(taskD, taskE)) ==>
        newDocument(taskA, taskD, taskE)
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
    }
  }
}
