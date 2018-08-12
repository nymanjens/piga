package flux.react.app.desktop

import common.OrderToken
import models.modification.EntityModification

private[desktop] case class Task(id: Long, orderToken: OrderToken, content: String, indentation: Int)
    extends Ordered[Task] {

  override def compare(that: Task): Int = {
    this.orderToken compare that.orderToken
  }
}
private[desktop] object Task {
  def withRandomId(orderToken: OrderToken, content: String, indentation: Int): Task = Task(
    id = EntityModification.generateRandomId(),
    orderToken = orderToken,
    content = content,
    indentation = indentation
  )
}
