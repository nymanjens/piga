package app.models.document

import app.models.access.ModelFields
import app.models.document.Task.FakeJsTaskEntity
import hydro.common.OrderToken
import hydro.common.time.Clock
import hydro.models.modification.EntityModification
import hydro.common.time.LocalDateTime
import hydro.models.UpdatableEntity.LastUpdateTime
import hydro.models.access.ModelField
import hydro.models.access.ModelField
import hydro.models.access.ModelField.any
import hydro.models.Entity
import hydro.models.UpdatableEntity

import scala.collection.immutable.Seq

final class Task private (jsTaskEntity: Task.FakeJsTaskEntity) extends Ordered[Task] {

  def id: Long = jsTaskEntity.id
  def content: TextWithMarkup = jsTaskEntity.content
  def orderToken: OrderToken = jsTaskEntity.orderToken
  def indentation: Int = jsTaskEntity.indentation
  def collapsed: Boolean = jsTaskEntity.collapsed
  def delayedUntil: Option[LocalDateTime] = jsTaskEntity.delayedUntil
  def tags: Seq[String] = jsTaskEntity.tags

  def contentString: String = content.contentString

  def hasUpdatesSinceCreation: Boolean = jsTaskEntity.lastUpdateTime != LastUpdateTime.neverUpdated

  def equalsIgnoringMetadata(that: Task): Boolean = {
    this.content == that.content &&
    this.orderToken == that.orderToken &&
    this.indentation == that.indentation &&
    this.collapsed == that.collapsed &&
    this.delayedUntil == that.delayedUntil &&
    this.tags == that.tags
  }

  def toTaskEntity: TaskEntity =
    TaskEntity(
      documentId = jsTaskEntity.documentId,
      contentHtml = jsTaskEntity.content.toHtml,
      orderToken = jsTaskEntity.orderToken,
      indentation = jsTaskEntity.indentation,
      collapsed = jsTaskEntity.collapsed,
      delayedUntil = jsTaskEntity.delayedUntil,
      tags = jsTaskEntity.tags,
      idOption = jsTaskEntity.idOption,
      lastUpdateTime = jsTaskEntity.lastUpdateTime,
    )

//  def merged(that: Task): Task = ???

//  @Deprecated def updated(content: TextWithMarkup = null,
//                          orderToken: OrderToken = null,
//                          indentation: Int = -1,
//                          collapsed: java.lang.Boolean = null,
//                          delayedUntil: Option[LocalDateTime] = null,
//                          tags: Seq[String] = null)(implicit clock: Clock): Task = {
//    val fieldMask = {
//      def ifUpdate(value: Any, currentValue: Any, field: ModelField[_, TaskEntity]): Seq[ModelField.any] =
//        value match {
//          case null | -1      => Seq()
//          case `currentValue` => Seq()
//          case _              => Seq(field)
//        }
//      ifUpdate(content, this.content, ModelFields.TaskEntity.contentHtml) ++
//        ifUpdate(orderToken, this.orderToken, ModelFields.TaskEntity.orderToken) ++
//        ifUpdate(indentation, this.indentation, ModelFields.TaskEntity.indentation) ++
//        ifUpdate(collapsed, this.collapsed, ModelFields.TaskEntity.collapsed) ++
//        ifUpdate(delayedUntil, this.delayedUntil, ModelFields.TaskEntity.delayedUntil) ++
//        ifUpdate(tags, this.tags, ModelFields.TaskEntity.tags)
//    }
//    new Task(
//      id = id,
//      content = Option(content) getOrElse this.content,
//      orderToken = Option(orderToken) getOrElse this.orderToken,
//      indentation = if (indentation == -1) this.indentation else indentation,
//      collapsed = if (collapsed == null) this.collapsed else collapsed,
//      delayedUntil = Option(delayedUntil) getOrElse this.delayedUntil,
//      tags = Option(tags) getOrElse this.tags,
//      lastUpdateTime = lastUpdateTime
//        .merge(LastUpdateTime.someFieldsUpdated(fieldMask, clock.nowInstant), forceIncrement = true),
//    )
//  }

  def copyWithId(newId: Long): Task = new Task(jsTaskEntity.copy(idValue = id))

  // **************** Ordered methods **************** //
  override def compare(that: Task): Int = {
    this.orderToken compare that.orderToken
  }
  // **************** Object methods **************** //
  override def equals(o: scala.Any): Boolean = o match {
    case that: Task => this.jsTaskEntity == that.jsTaskEntity
    case _          => false
  }
  override def hashCode(): Int = jsTaskEntity.hashCode()
}

object Task {
  val nullInstance: Task = new Task(
    Task.FakeJsTaskEntity(
      documentId = -1,
      content = TextWithMarkup.empty,
      orderToken = OrderToken.middle,
      indentation = 0,
      collapsed = false,
      delayedUntil = None,
      tags = Seq(),
      idValue = -1,
      lastUpdateTime = LastUpdateTime.neverUpdated,
    ))

  def withRandomId(content: TextWithMarkup,
                   orderToken: OrderToken,
                   indentation: Int,
                   collapsed: Boolean,
                   delayedUntil: Option[LocalDateTime],
                   tags: Seq[String])(implicit document: Document): Task =
    new Task(
      Task.FakeJsTaskEntity(
        documentId = document.id,
        content = content,
        orderToken = orderToken,
        indentation = indentation,
        collapsed = collapsed,
        delayedUntil = delayedUntil,
        tags = tags,
        idValue = EntityModification.generateRandomId(),
        lastUpdateTime = LastUpdateTime.neverUpdated,
      ))

  def fromTaskEntity(taskEntity: TaskEntity): Task =
    new Task(
      Task.FakeJsTaskEntity(
        documentId = taskEntity.documentId,
        content = TextWithMarkup.fromHtml(taskEntity.contentHtml),
        orderToken = taskEntity.orderToken,
        indentation = taskEntity.indentation,
        collapsed = taskEntity.collapsed,
        delayedUntil = taskEntity.delayedUntil,
        tags = taskEntity.tags,
        idValue = taskEntity.id,
        lastUpdateTime = taskEntity.lastUpdateTime,
      ))

  /**
    * Fake entity that provides a way to re-use UpdatableEntity logic without converting `TextWithMarkup` into
    * `String`.
    */
  private case class FakeJsTaskEntity(documentId: Long,
                                      content: TextWithMarkup,
                                      orderToken: OrderToken,
                                      indentation: Int,
                                      collapsed: Boolean,
                                      delayedUntil: Option[LocalDateTime],
                                      tags: Seq[String],
                                      idValue: Long,
                                      override val lastUpdateTime: LastUpdateTime =
                                        LastUpdateTime.neverUpdated,
  ) extends UpdatableEntity {
    override def idOption: Option[Long] = Some(id)
    override def withId(id: Long) = copy(idValue = id)
    override def withLastUpdateTime(time: LastUpdateTime): Entity = copy(lastUpdateTime = time)
  }
}
