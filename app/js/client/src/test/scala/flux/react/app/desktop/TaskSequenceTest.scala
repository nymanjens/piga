package flux.react.app.desktop

import common.testing.JsTestObjects._
import common.testing.TestModule
import flux.react.app.desktop.EditHistory.Edit
import flux.react.app.desktop.Document.{
  DetachedCursor,
  DetachedSelection,
  IndexedCursor,
  IndexedSelection
}
import scala2js.Converters._
import utest._

import scala.collection.immutable.Seq

object DocumentTest extends TestSuite {
  val taskBB = newTask("Task BB", orderToken = orderTokenB)
  val taskEE = newTask("Task EE", orderToken = orderTokenE)

  override def tests = TestSuite {
    "replaced" - {
      val document = createDocument(taskA, taskB, taskC)

      document.replaced(toReplace = Seq(taskB), toAdd = Seq(taskBB)) ==>
        createDocument(taskA, taskBB, taskC)
      document.replaced(toReplace = Seq(taskB, taskC), toAdd = Seq(taskD, taskE)) ==>
        createDocument(taskA, taskD, taskE)
    }
    "indexOf" - {
      val document = createDocument(taskA, taskB, taskBB, taskC, taskD, taskE, taskEE)

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
        implicit val document = createDocument(newTask("the red apple"))
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
        implicit val document = createDocument(task1, task2)
        val selection = IndexedSelection(IndexedCursor(0, 0), IndexedCursor(1, 5))

        selection.detach ==>
          DetachedSelection(DetachedCursor(task1, 0), DetachedCursor(task2, 5))
        selection.detach.attachToTasks ==> selection
      }
    }
  }

  private def createDocument(tasks: Task*): Document = new Document(Seq(tasks: _*))
}
