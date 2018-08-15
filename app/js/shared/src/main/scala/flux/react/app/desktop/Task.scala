package flux.react.app.desktop

import common.OrderToken
import common.ScalaUtils.visibleForTesting
import flux.react.app.desktop.Task.MarkupTag
import models.modification.EntityModification

import scala.collection.immutable.Seq

final class Task private (val id: Long,
                          val orderToken: OrderToken,
                          val contentTags: Seq[MarkupTag],
                          val indentation: Int)
    extends Ordered[Task] {

  override def compare(that: Task): Int = {
    this.orderToken compare that.orderToken
  }

  lazy val contentString: String = MarkupTag.contentString(contentTags)
}
object Task {
  def withRandomId(orderToken: OrderToken, contentTags: Seq[MarkupTag], indentation: Int): Task =
    new Task(
      id = EntityModification.generateRandomId(),
      orderToken = orderToken,
      contentTags = MarkupTag.canonicalize(contentTags),
      indentation = indentation
    )

  sealed trait MarkupTag
  object MarkupTag {
    def sub(tags: Seq[MarkupTag], beginOffset: Int, endOffset: Int = -1): Seq[MarkupTag] = ???
    @visibleForTesting private[desktop] def canonicalize(tags: Seq[MarkupTag]): Seq[MarkupTag] = {
      // TODO: Implement
      tags
    }
    def contentString(tags: Seq[MarkupTag]): String =
      tags.map {
        case MarkupTag.Text(text)     => text
        case MarkupTag.Bold(children) => contentString(children)
      }.mkString

    case class Text(text: String) extends MarkupTag
    case class Bold(children: Seq[MarkupTag]) extends MarkupTag
  }
}
