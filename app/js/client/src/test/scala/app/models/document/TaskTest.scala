package app.models.document

import app.common.testing.JsTestObjects.newDocument
import app.common.testing.TestObjects._
import utest._

import scala.collection.immutable.Seq

object TaskTest extends TestSuite {

  implicit private val document = newDocument()

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
          documentId = document.id,
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

      task1A.hashCode == task1B.hashCode ==> true
      task1A.hashCode == task2.hashCode ==> false
    }
    "equalsIgnoringMetadata" - {
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

      (taskA1 equalsIgnoringMetadata taskA2) ==> true
      (taskA1 equalsIgnoringMetadata taskB) ==> false
    }
  }
}
