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

  val taskA = Task.withRandomId(orderToken = orderTokenA, content = TextWithMarkup("abcd"), indentation = 1)
  val taskB = Task.withRandomId(orderToken = orderTokenB, content = TextWithMarkup("xyz"), indentation = 3)
  val testTask = taskA

  val testSelection = IndexedSelection.collapsed(IndexedCursor(2, 123))
  val testDetachedCursor = DetachedCursor(taskA, 12938)
  val testDetachedSelection = DetachedSelection(testDetachedCursor, testDetachedCursor)
}
