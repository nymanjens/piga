package flux.react.app.desktop

import common.OrderToken
import models.modification.EntityModification

final class Task private (val id: Long,
                          val orderToken: OrderToken,
                          val content: FlatMarkupText,
                          val indentation: Int)
    extends Ordered[Task] {

  override def compare(that: Task): Int = {
    this.orderToken compare that.orderToken
  }

  def contentString: String = content.contentString
}
object Task {
  def withRandomId(orderToken: OrderToken, contentTags: FlatMarkupText, indentation: Int): Task =
    new Task(
      id = EntityModification.generateRandomId(),
      orderToken = orderToken,
      content = FlatMarkupText.canonicalize(contentTags),
      indentation = indentation
    )
}
