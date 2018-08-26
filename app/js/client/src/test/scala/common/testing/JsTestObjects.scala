package common.testing

import common.OrderToken
import flux.react.app.desktop.TaskSequence.{
  DetachedCursor,
  DetachedSelection,
  IndexedCursor,
  IndexedSelection
}
import flux.react.app.desktop.{Task, TaskSequence, TextWithMarkup}

object JsTestObjects {

  val orderTokenA = OrderToken.middleBetween(None, Some(OrderToken.middle))
  val orderTokenB = OrderToken.middleBetween(Some(OrderToken.middle), None)
  val orderTokenC = OrderToken.middleBetween(Some(orderTokenB), None)
  val orderTokenD = OrderToken.middleBetween(Some(orderTokenC), None)
  val orderTokenE = OrderToken.middleBetween(Some(orderTokenD), None)

  val taskA = newTask("Task A", orderToken = orderTokenA, indentation = 2)
  val taskB = newTask("Task B", orderToken = orderTokenB, indentation = 3)
  val taskC = newTask("Task C", orderToken = orderTokenC)
  val taskD = newTask("Task D", orderToken = orderTokenD)
  val taskE = newTask("Task E", orderToken = orderTokenE)
  val testTask = taskA

  val testSelection = IndexedSelection.collapsed(IndexedCursor(2, 123))
  val testDetachedCursor = DetachedCursor(taskA, 12938)
  val testDetachedSelection = DetachedSelection(testDetachedCursor, testDetachedCursor)

  def newTask(contentString: String, orderToken: OrderToken = orderTokenB, indentation: Int = 1): Task =
    Task.withRandomId(
      orderToken = orderToken,
      content = TextWithMarkup(contentString),
      indentation = indentation)
}
