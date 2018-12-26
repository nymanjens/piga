package app.models.document

import common.OrderToken
import common.time.LocalDateTime
import app.models.modification.EntityModification

import scala.collection.immutable.Seq

final class Task private (val id: Long,
                          val content: TextWithMarkup,
                          val orderToken: OrderToken,
                          val indentation: Int,
                          val collapsed: Boolean,
                          val delayedUntil: Option[LocalDateTime],
                          val tags: Seq[String])
    extends Ordered[Task] {

  def contentString: String = content.contentString

  def equalsIgnoringId(that: Task): Boolean = {
    this.content == that.content &&
    this.orderToken == that.orderToken &&
    this.indentation == that.indentation &&
    this.collapsed == that.collapsed &&
    this.delayedUntil == that.delayedUntil &&
    this.tags == that.tags
  }

  def toTaskEntity(implicit document: Document): TaskEntity =
    TaskEntity(
      documentId = document.id,
      contentHtml = content.toHtml,
      orderToken = orderToken,
      indentation = indentation,
      collapsed = collapsed,
      delayedUntil = delayedUntil,
      tags = tags,
      idOption = Some(id)
    )

  def copyWithId(newId: Long): Task =
    new Task(
      id = newId,
      content = content,
      orderToken = orderToken,
      indentation = indentation,
      collapsed = collapsed,
      delayedUntil = delayedUntil,
      tags = tags)

  def copyWithRandomId(content: TextWithMarkup = null,
                       orderToken: OrderToken = null,
                       indentation: Int = -1,
                       collapsed: java.lang.Boolean = null,
                       delayedUntil: Option[LocalDateTime] = null,
                       tags: Seq[String] = null): Task = {
    Task.withRandomId(
      content = Option(content) getOrElse this.content,
      orderToken = Option(orderToken) getOrElse this.orderToken,
      indentation = if (indentation == -1) this.indentation else indentation,
      collapsed = if (collapsed == null) this.collapsed else collapsed,
      delayedUntil = Option(delayedUntil) getOrElse this.delayedUntil,
      tags = Option(tags) getOrElse this.tags
    )
  }

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
      tags = tags
    )

  def fromTaskEntity(taskEntity: TaskEntity): Task =
    new Task(
      id = taskEntity.id,
      content = TextWithMarkup.fromHtml(taskEntity.contentHtml),
      orderToken = taskEntity.orderToken,
      indentation = taskEntity.indentation,
      collapsed = taskEntity.collapsed,
      delayedUntil = taskEntity.delayedUntil,
      tags = taskEntity.tags
    )
}
