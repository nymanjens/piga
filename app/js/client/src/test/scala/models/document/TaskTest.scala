package models.document

import common.testing.TestObjects._
import scala2js.Converters._
import utest._

object TaskTest extends TestSuite {

  override def tests = TestSuite {
    "equals and hashCode" - {
      val task1A = Task.withRandomId(content = TextWithMarkup("a"), orderToken = orderTokenA, indentation = 1)
      val task1B = Task.fromTaskEntity(
        TaskEntity(
          documentId = 123,
          contentHtml = "a",
          orderToken = orderTokenA,
          indentation = 1,
          idOption = Some(task1A.id)))
      val task2 = Task.withRandomId(content = TextWithMarkup("a"), orderToken = orderTokenA, indentation = 1)

      task1A == task1B ==> true
      task1A == task2 ==> false

      task1A.hashCode() == task1B.hashCode() ==> true
      task1A.hashCode() == task2.hashCode() ==> false
    }
    "equalsIgnoringId" - {
      val taskA1 = Task.withRandomId(content = TextWithMarkup("a"), orderToken = orderTokenA, indentation = 1)
      val taskA2 = Task.withRandomId(content = TextWithMarkup("a"), orderToken = orderTokenA, indentation = 1)
      val taskB = Task.withRandomId(content = TextWithMarkup("b"), orderToken = orderTokenA, indentation = 1)

      (taskA1 equalsIgnoringId taskA2) ==> true
      (taskA1 equalsIgnoringId taskB) ==> false
    }
    "copyWithRandomId" - {
      val task = Task.withRandomId(content = TextWithMarkup("a"), orderToken = orderTokenA, indentation = 1)

      "without any other changes" - {
        val newTask = task.copyWithRandomId()
        assert(newTask.id != task.id)
        newTask.content ==> task.content
        newTask.orderToken ==> task.orderToken
        newTask.indentation ==> task.indentation
      }
      "with indentation changes" - {
        val newTask = task.copyWithRandomId(indentation = 7)
        assert(newTask.id != task.id)
        newTask.content ==> task.content
        newTask.orderToken ==> task.orderToken
        newTask.indentation ==> 7
      }
      "with all changes" - {
        val newTask =
          task.copyWithRandomId(content = TextWithMarkup("x"), orderToken = orderTokenB, indentation = 7)
        assert(newTask.id != task.id)
        newTask.content ==> TextWithMarkup("x")
        newTask.orderToken ==> orderTokenB
        newTask.indentation ==> 7
      }
    }
  }
}
