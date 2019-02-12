package app.common.testing

import hydro.common.OrderToken
import app.common.testing.TestObjects._
import app.models.document.Document.DetachedCursor
import app.models.document.Document.DetachedSelection
import app.models.document.Document.IndexedCursor
import app.models.document.Document.IndexedSelection
import app.models.document.Document
import app.models.document.Task
import app.models.document.TextWithMarkup
import hydro.common.time.LocalDateTime

import scala.collection.immutable.Seq

object JsTestObjects {

  def taskA = newTask("Task A", orderToken = orderTokenA, indentation = 2)
  def taskB = newTask("Task B", orderToken = orderTokenB, indentation = 3)
  def taskC = newTask("Task C", orderToken = orderTokenC)
  def taskD = newTask("Task D", orderToken = orderTokenD)
  def taskE = newTask("Task E", orderToken = orderTokenE)
  def testTask = taskA

  def testSelection = IndexedSelection.singleton(IndexedCursor(2, 123))
  def testDetachedCursor = DetachedCursor(taskA, 12938)
  def testDetachedSelection = DetachedSelection(testDetachedCursor, testDetachedCursor)

  def newTask(contentString: String = null,
              content: TextWithMarkup = null,
              orderToken: OrderToken = orderTokenB,
              indentation: Int = 1,
              collapsed: Boolean = false,
              delayedUntil: Option[LocalDateTime] = Some(testDate),
              tags: Seq[String] = Seq("test-tag")): Task = {
    require(content != null || contentString != null)
    Task.withRandomId(
      content = Option(content) getOrElse TextWithMarkup(contentString),
      orderToken = orderToken,
      indentation = indentation,
      collapsed = collapsed,
      delayedUntil = delayedUntil,
      tags = tags
    )
  }

  def newDocument(tasks: Task*): Document =
    new Document(id = 12873, name = "test document", orderToken = orderTokenA, tasks = Seq(tasks: _*))
}
