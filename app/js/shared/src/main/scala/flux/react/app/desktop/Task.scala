package flux.react.app.desktop

import java.util.Objects

import common.OrderToken
import models.modification.EntityModification

final class Task private (val id: Long,
                          val orderToken: OrderToken,
                          val content: TextWithMarkup,
                          val indentation: Int)
    extends Ordered[Task] {

  override def compare(that: Task): Int = {
    this.orderToken compare that.orderToken
  }

  def contentString: String = content.contentString

  def equalsIgnoringId(that: Task): Boolean = {
    this.orderToken == that.orderToken && this.content == that.content && this.indentation == that.indentation
  }

  override def toString: String = s"Task($id, $contentString)"
  override def equals(o: scala.Any): Boolean = o match {
    case that: Task => this.id == that.id
    case _          => false
  }
  override def hashCode(): Int = id.hashCode()
}
object Task {
  def withRandomId(orderToken: OrderToken, content: TextWithMarkup, indentation: Int): Task =
    new Task(
      id = EntityModification.generateRandomId(),
      orderToken = orderToken,
      content = content,
      indentation = indentation
    )
}
