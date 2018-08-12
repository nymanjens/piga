package flux.react.app.desktop

import common.OrderToken
import models.modification.EntityModification

private[desktop] final class Task private (val id: Long,
                                           val orderToken: OrderToken,
                                           val content: String,
                                           val indentation: Int)
    extends Ordered[Task] {

  override def compare(that: Task): Int = {
    this.orderToken compare that.orderToken
  }
}
private[desktop] object Task {
  def withRandomId(orderToken: OrderToken, content: String, indentation: Int): Task =
    new Task(
      id = EntityModification.generateRandomId(),
      orderToken = orderToken,
      content = content,
      indentation = indentation
    )
}
