package app.models.access

import app.models.document.DocumentEntity
import app.models.document.DocumentPermissionAndPlacement
import app.models.document.TaskEntity
import app.models.user.User
import hydro.common.GuavaReplacement.ImmutableBiMap
import hydro.common.OrderToken
import hydro.common.time.LocalDateTime
import hydro.common.CollectionUtils
import hydro.common.ScalaUtils
import hydro.models.modification.EntityType
import hydro.models.Entity
import hydro.models.access.ModelField

import scala.collection.immutable.Seq

object ModelFields {
  // **************** Methods **************** //
  def id[E <: Entity](implicit entityType: EntityType[E]): ModelField[Long, E] = entityType match {
    case app.models.user.User.Type               => User.id.asInstanceOf[ModelField[Long, E]]
    case app.models.document.DocumentEntity.Type => DocumentEntity.id.asInstanceOf[ModelField[Long, E]]
    case app.models.document.DocumentPermissionAndPlacement.Type =>
      DocumentPermissionAndPlacement.id.asInstanceOf[ModelField[Long, E]]
    case app.models.document.TaskEntity.Type => TaskEntity.id.asInstanceOf[ModelField[Long, E]]
  }

  // **************** Enumeration of all fields **************** //
  object User {
    private type E = User

    val id = ModelField.forId[E]()
    val loginName: ModelField[String, E] = ModelField("loginName", _.loginName, v => _.copy(loginName = v))
    val passwordHash: ModelField[String, E] =
      ModelField("passwordHash", _.passwordHash, v => _.copy(passwordHash = v))
    val name: ModelField[String, E] = ModelField("name", _.name, v => _.copy(name = v))
    val isAdmin: ModelField[Boolean, E] = ModelField("isAdmin", _.isAdmin, v => _.copy(isAdmin = v))
  }

  object DocumentEntity {
    private type E = DocumentEntity

    val id = ModelField.forId[E]()
    val name: ModelField[String, E] = ModelField("name", _.name, v => _.copy(name = v))
  }

  object DocumentPermissionAndPlacement {
    private type E = DocumentPermissionAndPlacement

    val id = ModelField.forId[E]()
    val documentId: ModelField[Long, E] = ModelField("documentId", _.documentId, v => _.copy(documentId = v))
    val userId: ModelField[Long, E] = ModelField("userId", _.userId, v => _.copy(userId = v))
    val orderToken: ModelField[OrderToken, E] =
      ModelField("orderToken", _.orderToken, v => _.copy(orderToken = v))
  }

  object TaskEntity {
    private type E = TaskEntity

    val id = ModelField.forId[E]()
    val documentId: ModelField[Long, E] = ModelField("documentId", _.documentId, v => _.copy(documentId = v))
    val contentHtml: ModelField[String, E] =
      ModelField("contentHtml", _.contentHtml, v => _.copy(contentHtml = v))
    val orderToken: ModelField[OrderToken, E] =
      ModelField("orderToken", _.orderToken, v => _.copy(orderToken = v))
    val indentation: ModelField[Int, E] =
      ModelField("indentation", _.indentation, v => _.copy(indentation = v))
    val collapsed: ModelField[Boolean, E] = ModelField("collapsed", _.collapsed, v => _.copy(collapsed = v))
    val checked: ModelField[Boolean, E] = ModelField("checked", _.checked, v => _.copy(checked = v))
    val delayedUntil: ModelField[Option[LocalDateTime], E] =
      ModelField("delayedUntil", _.delayedUntil, v => _.copy(delayedUntil = v))
    val tags: ModelField[Seq[String], E] = ModelField("tags", _.tags, v => _.copy(tags = v))
    val lastContentModifierUserId: ModelField[Long, E] = ModelField(
      "lastContentModifierUserId",
      _.lastContentModifierUserId,
      v => _.copy(lastContentModifierUserId = v),
    )
  }

  // **************** Field-related methods **************** //
  private val allFields: Seq[ModelField.any] = Seq(
    User.id,
    User.loginName,
    User.passwordHash,
    User.name,
    User.isAdmin,
    DocumentEntity.id,
    DocumentEntity.name,
    DocumentPermissionAndPlacement.id,
    DocumentPermissionAndPlacement.documentId,
    DocumentPermissionAndPlacement.userId,
    DocumentPermissionAndPlacement.orderToken,
    TaskEntity.id,
    TaskEntity.documentId,
    TaskEntity.contentHtml,
    TaskEntity.orderToken,
    TaskEntity.indentation,
    TaskEntity.collapsed,
    TaskEntity.checked,
    TaskEntity.delayedUntil,
    TaskEntity.tags,
    TaskEntity.lastContentModifierUserId,
  )
  private val fieldToNumberMap: ImmutableBiMap[ModelField.any, Int] =
    CollectionUtils.toBiMapWithStableIntKeys(
      stableNameMapper = field => s"${field.entityType.entityClass.getSimpleName}$$${field.name}$$",
      values = allFields,
    )

  def allFieldsOfEntity(entityType: EntityType.any): Seq[ModelField.any] = {
    allFields.filter(_.entityType == entityType).toVector
  }
  def toNumber(field: ModelField.any): Int = fieldToNumberMap.get(field)
  def fromNumber(number: Int): ModelField.any = fieldToNumberMap.inverse().get(number)
}
