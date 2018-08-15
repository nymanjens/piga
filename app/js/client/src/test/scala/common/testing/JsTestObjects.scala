package common.testing

import common.OrderToken
import flux.react.app.desktop.{Task, TaskSequence}

object JsTestObjects {

  val orderTokenA = OrderToken.middleBetween(None, Some(OrderToken.middle))
  val orderTokenB = OrderToken.middleBetween(Some(OrderToken.middle), None)

  val taskA = Task.withRandomId(orderToken = orderTokenA, contentTags = "abcd", indentation = 1)
  val taskB = Task.withRandomId(orderToken = orderTokenB, contentTags = "xyz", indentation = 3)
}
