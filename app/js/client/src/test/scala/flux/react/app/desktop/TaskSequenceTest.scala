package flux.react.app.desktop

import common.testing.JsTestObjects._
import common.testing.TestModule
import flux.react.app.desktop.EditHistory.Edit
import flux.react.app.desktop.TaskSequence.{
  DetachedCursor,
  DetachedSelection,
  IndexedCursor,
  IndexedSelection
}
import scala2js.Converters._
import utest._

import scala.collection.immutable.Seq

object TaskSequenceTest extends TestSuite {
  val taskBB = newTask("Task BB", orderToken = orderTokenB)
  val taskEE = newTask("Task EE", orderToken = orderTokenE)

  override def tests = TestSuite {
    "replaced" - {
      val taskSequence = createTaskSequence(taskA, taskB, taskC)

      taskSequence.replaced(toReplace = Seq(taskB), toAdd = Seq(taskBB)) ==>
        createTaskSequence(taskA, taskBB, taskC)
      taskSequence.replaced(toReplace = Seq(taskB, taskC), toAdd = Seq(taskD, taskE)) ==>
        createTaskSequence(taskA, taskD, taskE)
    }
    "indexOf" - {
      val taskSequence = createTaskSequence(taskA, taskB, taskBB, taskC, taskD, taskE, taskEE)

      taskSequence.indexOf(taskA) ==> 0
      taskSequence.indexOf(taskB) ==> 1
      taskSequence.indexOf(taskBB) ==> 2
      taskSequence.indexOf(taskC) ==> 3
      taskSequence.indexOf(taskD) ==> 4
      taskSequence.indexOf(taskE) ==> 5
      taskSequence.indexOf(taskEE) ==> 6
    }
    "IndexedCursor" - {
      "plusWord" - {
        implicit val taskSequence = createTaskSequence(newTask("the red apple"))
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
        implicit val taskSequence = createTaskSequence(task1, task2)
        val selection = IndexedSelection(IndexedCursor(0, 0), IndexedCursor(1, 5))

        selection.detach ==>
          DetachedSelection(DetachedCursor(task1, 0), DetachedCursor(task2, 5))
        selection.detach.attachToTasks ==> selection
      }
    }
  }

  private def createTaskSequence(tasks: Task*): TaskSequence = new TaskSequence(Seq(tasks: _*))
}
