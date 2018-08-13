package common.testing

import common.OrderToken
import flux.react.app.desktop.{Task, TaskSequence}

object JsTestObjects {

  val orderTokenA = OrderToken.middleBetween(None, Some(OrderToken.middle))
  val orderTokenB = OrderToken.middleBetween(Some(OrderToken.middle), None)

  val taskA = Task.withRandomId(orderToken = orderTokenA, content = "abcd", indentation = 1)
  val taskB = Task.withRandomId(orderToken = orderTokenB, content = "xyz", indentation = 3)
}
