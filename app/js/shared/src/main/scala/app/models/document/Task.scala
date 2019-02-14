package app.models.document

import app.models.access.ModelFields
import hydro.common.OrderToken
import hydro.common.time.Clock
import hydro.models.modification.EntityModification
import hydro.common.time.LocalDateTime
import hydro.models.UpdatableEntity.LastUpdateTime
import hydro.models.access.ModelField
import hydro.models.access.ModelField

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

  def equalsIgnoringMetadata(that: Task): Boolean = {
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
      idOption = Some(id),
      lastUpdateTime = lastUpdateTime,
    )

  def updated(content: TextWithMarkup = null,
              orderToken: OrderToken = null,
              indentation: Int = -1,
              collapsed: java.lang.Boolean = null,
              delayedUntil: Option[LocalDateTime] = null,
              tags: Seq[String] = null)(implicit clock: Clock): Task = {
    val fieldMask: Seq[ModelField[_, TaskEntity]] = {
      def ifUpdate(value: Any, currentValue: Any, field: ModelField[_, TaskEntity]) = value match {
        case null | -1      => Seq()
        case `currentValue` => Seq()
        case _              => Seq(field)
      }
      ifUpdate(content, this.content, ModelFields.TaskEntity.contentHtml) ++
        ifUpdate(orderToken, this.orderToken, ModelFields.TaskEntity.orderToken) ++
        ifUpdate(indentation, this.indentation, ModelFields.TaskEntity.indentation) ++
        ifUpdate(collapsed, this.collapsed, ModelFields.TaskEntity.collapsed) ++
        ifUpdate(delayedUntil, this.delayedUntil, ModelFields.TaskEntity.delayedUntil) ++
        ifUpdate(tags, this.tags, ModelFields.TaskEntity.tags)
    }
    new Task(
      id = id,
      content = Option(content) getOrElse this.content,
      orderToken = Option(orderToken) getOrElse this.orderToken,
      indentation = if (indentation == -1) this.indentation else indentation,
      collapsed = if (collapsed == null) this.collapsed else collapsed,
      delayedUntil = Option(delayedUntil) getOrElse this.delayedUntil,
      tags = Option(tags) getOrElse this.tags,
      lastUpdateTime = lastUpdateTime
        .merge(LastUpdateTime.someFieldsUpdated(fieldMask, clock.nowInstant), forceIncrement = true),
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
