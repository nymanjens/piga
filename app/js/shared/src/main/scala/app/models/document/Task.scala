package app.models.document

import app.models.access.ModelFields
import app.models.document.DocumentEdit.MaskedTaskUpdate
import app.models.document.DocumentEdit.MaskedTaskUpdate.FieldUpdate
import app.models.document.Task.FakeJsTaskEntity
import app.models.user.User
import hydro.common.OrderToken
import hydro.common.time.Clock
import hydro.common.time.LocalDateTime
import hydro.common.GuavaReplacement.ImmutableBiMap
import hydro.models.modification.EntityModification
import hydro.models.UpdatableEntity.LastUpdateTime
import hydro.models.access.ModelField
import hydro.models.Entity
import hydro.models.UpdatableEntity
import hydro.models.access.ModelField.FieldType
import hydro.models.access.ModelField.IdModelField
import hydro.models.modification.EntityType

import scala.collection.immutable.Seq
import scala.collection.mutable

final class Task private (private val jsTaskEntity: Task.FakeJsTaskEntity) extends Ordered[Task] {

  def id: Long = jsTaskEntity.id
  def content: TextWithMarkup = jsTaskEntity.content
  def orderToken: OrderToken = jsTaskEntity.orderToken
  def indentation: Int = jsTaskEntity.indentation
  def collapsed: Boolean = jsTaskEntity.collapsed
  def delayedUntil: Option[LocalDateTime] = jsTaskEntity.delayedUntil
  def tags: Seq[String] = jsTaskEntity.tags
  def lastContentModifierUserId: Long = jsTaskEntity.lastContentModifierUserId

  def contentString: String = content.contentString

  def equalsIgnoringMetadata(that: Task): Boolean = {
    this.content == that.content &&
    this.orderToken == that.orderToken &&
    this.indentation == that.indentation &&
    this.collapsed == that.collapsed &&
    this.delayedUntil == that.delayedUntil &&
    this.tags == that.tags &&
    this.lastContentModifierUserId == that.lastContentModifierUserId
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
      lastContentModifierUserId = jsTaskEntity.lastContentModifierUserId,
      idOption = jsTaskEntity.idOption,
      lastUpdateTime = jsTaskEntity.lastUpdateTime.copy(timePerField = {
        for ((fakeField, time) <- jsTaskEntity.lastUpdateTime.timePerField)
          yield FakeJsTaskEntity.fakeToEntityFieldBiMap.get(fakeField) -> time
      }.toMap),
    )

  def mergedWith(that: Task): Task = new Task(UpdatableEntity.merge(this.jsTaskEntity, that.jsTaskEntity))

  def withAppliedUpdateAndNewUpdateTime(maskedTaskUpdate: MaskedTaskUpdate)(implicit clock: Clock): Task = {
    var taskWithUpdatedFields = jsTaskEntity
    val fieldMask = mutable.Buffer[ModelField[_, FakeJsTaskEntity]]()

    def applyUpdate[V](
        fieldUpdate: Option[FieldUpdate[V]],
        modelField: ModelField[V, FakeJsTaskEntity],
    ): Unit = {
      if (fieldUpdate.isDefined) {
        taskWithUpdatedFields = modelField.set(taskWithUpdatedFields, fieldUpdate.get.newValue)
        fieldMask.append(modelField)
      }
    }
    applyUpdate(maskedTaskUpdate.content, FakeJsTaskEntity.Fields.content)
    applyUpdate(maskedTaskUpdate.orderToken, FakeJsTaskEntity.Fields.orderToken)
    applyUpdate(maskedTaskUpdate.indentation, FakeJsTaskEntity.Fields.indentation)
    applyUpdate(maskedTaskUpdate.collapsed, FakeJsTaskEntity.Fields.collapsed)
    applyUpdate(maskedTaskUpdate.delayedUntil, FakeJsTaskEntity.Fields.delayedUntil)
    applyUpdate(maskedTaskUpdate.tags, FakeJsTaskEntity.Fields.tags)
    applyUpdate(maskedTaskUpdate.lastContentModifierUserId, FakeJsTaskEntity.Fields.lastContentModifierUserId)

    require(fieldMask.nonEmpty, s"Empty fieldMask for maskedTaskUpdate: $maskedTaskUpdate")
    val modification = EntityModification.createUpdate(taskWithUpdatedFields, fieldMask.toVector)
    new Task(modification.updatedEntity)
  }

  def copyWithId(newId: Long): Task = new Task(jsTaskEntity.copy(idValue = newId))

  def copyForTests(content: TextWithMarkup = null,
                   orderToken: OrderToken = null,
                   indentation: Int = -1,
                   collapsed: java.lang.Boolean = null,
                   delayedUntil: Option[LocalDateTime] = null,
                   tags: Seq[String] = null,
  ): Task = {
    def toScala(bool: java.lang.Boolean): Boolean = bool
    new Task(
      Task.FakeJsTaskEntity(
        documentId = this.jsTaskEntity.documentId,
        content = Option(content) getOrElse this.content,
        orderToken = Option(orderToken) getOrElse this.orderToken,
        indentation = if (indentation == -1) this.indentation else indentation,
        collapsed = toScala(Option(collapsed) getOrElse this.collapsed),
        delayedUntil = Option(delayedUntil) getOrElse this.delayedUntil,
        tags = Option(tags) getOrElse this.tags,
        lastContentModifierUserId = jsTaskEntity.lastContentModifierUserId,
        idValue = this.id,
        lastUpdateTime = this.jsTaskEntity.lastUpdateTime,
      ))
  }

  // **************** Ordered methods **************** //
  override def compare(that: Task): Int = {
    this.orderToken compare that.orderToken
  }
  // **************** Object methods **************** //
  override def toString: String = jsTaskEntity.toString

  override def equals(o: scala.Any): Boolean = o match {
    // Heuristic: Rely solely on the hashCode and accept unlikely collisions
    case that: Task => this.hashCode == that.hashCode
    case _          => false
  }
  override lazy val hashCode: Int = jsTaskEntity.hashCode()
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
      lastContentModifierUserId = -1,
      idValue = -1,
      lastUpdateTime = LastUpdateTime.neverUpdated,
    ))

  def withRandomId(
      content: TextWithMarkup,
      orderToken: OrderToken,
      indentation: Int,
      collapsed: Boolean,
      delayedUntil: Option[LocalDateTime],
      tags: Seq[String],
  )(implicit document: Document, user: User): Task =
    new Task(
      Task.FakeJsTaskEntity(
        documentId = document.id,
        content = content,
        orderToken = orderToken,
        indentation = indentation,
        collapsed = collapsed,
        delayedUntil = delayedUntil,
        tags = tags,
        lastContentModifierUserId = user.id,
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
        lastContentModifierUserId = taskEntity.lastContentModifierUserId,
        idValue = taskEntity.id,
        lastUpdateTime = taskEntity.lastUpdateTime.copy(timePerField = {
          for ((entityField, time) <- taskEntity.lastUpdateTime.timePerField)
            yield FakeJsTaskEntity.fakeToEntityFieldBiMap.inverse().get(entityField) -> time
        }.toMap),
      ))

  /**
    * Fake entity that provides a way to re-use UpdatableEntity logic without converting `TextWithMarkup` into
    * `String`.
    */
  private case class FakeJsTaskEntity(
      documentId: Long,
      content: TextWithMarkup,
      orderToken: OrderToken,
      indentation: Int,
      collapsed: Boolean,
      delayedUntil: Option[LocalDateTime],
      tags: Seq[String],
      lastContentModifierUserId: Long,
      idValue: Long,
      override val lastUpdateTime: LastUpdateTime,
  ) extends UpdatableEntity {
    override def idOption: Option[Long] = Some(idValue)
    override def withId(id: Long) = copy(idValue = id)
    override def withLastUpdateTime(time: LastUpdateTime): Entity = copy(lastUpdateTime = time)
  }
  private object FakeJsTaskEntity {
    implicit val Type: EntityType[FakeJsTaskEntity] = EntityType()

    val fakeToEntityFieldBiMap: ImmutableBiMap[ModelField.any, ModelField.any] =
      ImmutableBiMap
        .builder[ModelField.any, ModelField.any]()
        .put(Fields.id, ModelFields.TaskEntity.id)
        .put(Fields.documentId, ModelFields.TaskEntity.documentId)
        .put(Fields.content, ModelFields.TaskEntity.contentHtml)
        .put(Fields.orderToken, ModelFields.TaskEntity.orderToken)
        .put(Fields.indentation, ModelFields.TaskEntity.indentation)
        .put(Fields.collapsed, ModelFields.TaskEntity.collapsed)
        .put(Fields.delayedUntil, ModelFields.TaskEntity.delayedUntil)
        .put(Fields.tags, ModelFields.TaskEntity.tags)
        .put(Fields.lastContentModifierUserId, ModelFields.TaskEntity.lastContentModifierUserId)
        .build()

    object Fields {
      private type E = FakeJsTaskEntity
      implicit private val textWithMarkupFieldType: FieldType[TextWithMarkup] = null

      case object id extends IdModelField[E]
      case object documentId
          extends ModelField[Long, E]("documentId", _.documentId, v => _.copy(documentId = v))
      case object content
          extends ModelField[TextWithMarkup, E]("content", _.content, v => _.copy(content = v))
      case object orderToken
          extends ModelField[OrderToken, E]("orderToken", _.orderToken, v => _.copy(orderToken = v))
      case object indentation
          extends ModelField[Int, E]("indentation", _.indentation, v => _.copy(indentation = v))
      case object collapsed
          extends ModelField[Boolean, E]("collapsed", _.collapsed, v => _.copy(collapsed = v))
      case object delayedUntil
          extends ModelField[Option[LocalDateTime], E](
            "delayedUntil",
            _.delayedUntil,
            v => _.copy(delayedUntil = v))
      case object tags extends ModelField[Seq[String], E]("tags", _.tags, v => _.copy(tags = v))
      case object lastContentModifierUserId
          extends ModelField[Long, E](
            "lastContentModifierUserId",
            _.lastContentModifierUserId,
            v => _.copy(lastContentModifierUserId = v))
    }
  }
}
