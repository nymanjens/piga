package app.models.document

import hydro.common.OrderToken
import hydro.models.modification.EntityModification
import hydro.common.time.LocalDateTime
import hydro.models.UpdatableEntity.LastUpdateTime

import scala.collection.immutable.Seq

final class Task private (val id: Long,
                          val content: TextWithMarkup,
                          val orderToken: OrderToken,
                          val indentation: Int,
                          val collapsed: Boolean,
                          val delayedUntil: Option[LocalDateTime],
                          val tags: Seq[String],
                          lastUpdateTime: LastUpdateTime,
) extends Ordered[Task] {

  def contentString: String = content.contentString

  def toTaskEntity(implicit document: Document): TaskEntity =
    TaskEntity(
      documentId = document.id,
      contentHtml = content.toHtml,
      orderToken = orderToken,
      indentation = indentation,
      collapsed = collapsed,
      delayedUntil = delayedUntil,
      tags = tags,
      idOption = Some(id),
      lastUpdateTime = lastUpdateTime,
    )

  // **************** Ordered methods **************** //
  override def compare(that: Task): Int = {
    this.orderToken compare that.orderToken
  }

  // **************** Object methods **************** //
  override def toString: String = s"Task($id, $contentString)"
  override def equals(o: scala.Any): Boolean = o match {
    case that: Task => this.id == that.id
    case _          => false
  }
  override def hashCode(): Int = id.hashCode()
}
object Task {
  def withRandomId(content: TextWithMarkup,
                   orderToken: OrderToken,
                   indentation: Int,
                   collapsed: Boolean,
                   delayedUntil: Option[LocalDateTime],
                   tags: Seq[String]): Task =
    new Task(
      id = EntityModification.generateRandomId(),
      content = content,
      orderToken = orderToken,
      indentation = indentation,
      collapsed = collapsed,
      delayedUntil = delayedUntil,
      tags = tags,
      lastUpdateTime = LastUpdateTime.neverUpdated,
    )

  def fromTaskEntity(taskEntity: TaskEntity): Task =
    new Task(
      id = taskEntity.id,
      content = TextWithMarkup.fromHtml(taskEntity.contentHtml),
      orderToken = taskEntity.orderToken,
      indentation = taskEntity.indentation,
      collapsed = taskEntity.collapsed,
      delayedUntil = taskEntity.delayedUntil,
      tags = taskEntity.tags,
      lastUpdateTime = taskEntity.lastUpdateTime,
    )
}
