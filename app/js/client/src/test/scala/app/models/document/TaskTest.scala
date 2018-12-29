package app.models.document

import app.common.testing.TestObjects._
import hydro.scala2js.StandardConverters._
import app.scala2js.AppConverters._
import utest._
import scala.collection.immutable.Seq

object TaskTest extends TestSuite {

  override def tests = TestSuite {
    "equals and hashCode" - {
      val task1A = Task.withRandomId(
        content = TextWithMarkup("a"),
        orderToken = orderTokenA,
        indentation = 1,
        collapsed = true,
        delayedUntil = Some(testDate),
        tags = Seq("a"))
      val task1B = Task.fromTaskEntity(
        TaskEntity(
          documentId = 123,
          contentHtml = "a",
          orderToken = orderTokenA,
          indentation = 1,
          collapsed = true,
          delayedUntil = Some(testDate),
          tags = Seq("a"),
          idOption = Some(task1A.id)
        ))
      val task2 = Task.withRandomId(
        content = TextWithMarkup("a"),
        orderToken = orderTokenA,
        indentation = 1,
        collapsed = true,
        delayedUntil = Some(testDate),
        tags = Seq("a"))

      task1A == task1B ==> true
      task1A == task2 ==> false

      task1A.hashCode() == task1B.hashCode() ==> true
      task1A.hashCode() == task2.hashCode() ==> false
    }
    "equalsIgnoringId" - {
      val taskA1 = Task.withRandomId(
        content = TextWithMarkup("a"),
        orderToken = orderTokenA,
        indentation = 1,
        collapsed = true,
        delayedUntil = Some(testDate),
        tags = Seq("a"))
      val taskA2 = Task.withRandomId(
        content = TextWithMarkup("a"),
        orderToken = orderTokenA,
        indentation = 1,
        collapsed = true,
        delayedUntil = Some(testDate),
        tags = Seq("a"))
      val taskB = Task.withRandomId(
        content = TextWithMarkup("b"),
        orderToken = orderTokenA,
        indentation = 1,
        collapsed = true,
        delayedUntil = Some(testDate),
        tags = Seq("a"))

      (taskA1 equalsIgnoringId taskA2) ==> true
      (taskA1 equalsIgnoringId taskB) ==> false
    }
    "copyWithRandomId" - {
      val task = Task.withRandomId(
        content = TextWithMarkup("a"),
        orderToken = orderTokenA,
        indentation = 1,
        collapsed = true,
        delayedUntil = Some(testDate),
        tags = Seq("a"))

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
          task.copyWithRandomId(
            content = TextWithMarkup("x"),
            orderToken = orderTokenB,
            indentation = 7,
            collapsed = false,
            delayedUntil = None,
            tags = Seq("x"))
        assert(newTask.id != task.id)
        newTask.content ==> TextWithMarkup("x")
        newTask.orderToken ==> orderTokenB
        newTask.indentation ==> 7
        newTask.collapsed ==> false
        newTask.delayedUntil ==> None
        newTask.tags ==> Seq("x")
      }
    }
  }
}
