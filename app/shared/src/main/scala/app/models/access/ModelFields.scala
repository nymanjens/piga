package app.models.access

import app.models.document.DocumentEntity
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
import hydro.models.access.ModelField.IdModelField

import scala.collection.immutable.Seq

object ModelFields {
  // **************** Methods **************** //
  def id[E <: Entity](implicit entityType: EntityType[E]): ModelField[Long, E] = entityType match {
    case app.models.user.User.Type               => User.id.asInstanceOf[ModelField[Long, E]]
    case app.models.document.DocumentEntity.Type => DocumentEntity.id.asInstanceOf[ModelField[Long, E]]
    case app.models.document.TaskEntity.Type     => TaskEntity.id.asInstanceOf[ModelField[Long, E]]
  }

  // **************** Enumeration of all fields **************** //
  object User {
    private type E = User

    case object id extends IdModelField[E]
    case object loginName extends ModelField[String, E]("loginName", _.loginName, v => _.copy(loginName = v))
    case object passwordHash
        extends ModelField[String, E]("passwordHash", _.passwordHash, v => _.copy(passwordHash = v))
    case object name extends ModelField[String, E]("name", _.name, v => _.copy(name = v))
    case object isAdmin extends ModelField[Boolean, E]("isAdmin", _.isAdmin, v => _.copy(isAdmin = v))
  }

  object DocumentEntity {
    private type E = DocumentEntity

    case object id extends IdModelField[E]
    case object name extends ModelField[String, E]("name", _.name, v => _.copy(name = v))
    case object orderToken
        extends ModelField[OrderToken, E]("orderToken", _.orderToken, v => _.copy(orderToken = v))
  }

  object TaskEntity {
    private type E = TaskEntity

    case object id extends IdModelField[E]
    case object documentId
        extends ModelField[Long, E]("documentId", _.documentId, v => _.copy(documentId = v))
    case object contentHtml
        extends ModelField[String, E]("contentHtml", _.contentHtml, v => _.copy(contentHtml = v))
    case object orderToken
        extends ModelField[OrderToken, E]("orderToken", _.orderToken, v => _.copy(orderToken = v))
    case object indentation
        extends ModelField[Int, E]("indentation", _.indentation, v => _.copy(indentation = v))
    case object collapsed extends ModelField[Boolean, E]("collapsed", _.collapsed, v => _.copy(collapsed = v))
    case object delayedUntil
        extends ModelField[Option[LocalDateTime], E](
          "delayedUntil",
          _.delayedUntil,
          v => _.copy(delayedUntil = v))
    case object tags extends ModelField[Seq[String], E]("tags", _.tags, v => _.copy(tags = v))
  }

  // **************** Field numbers **************** //
  private val fieldToNumberMap: ImmutableBiMap[ModelField.any, Int] =
    CollectionUtils.toBiMapWithStableIntKeys(
      stableNameMapper = field =>
        ScalaUtils.stripRequiredPrefix(field.getClass.getName, prefix = ModelFields.getClass.getName),
      values = Seq(
        User.id,
        User.loginName,
        User.passwordHash,
        User.name,
        User.isAdmin,
        DocumentEntity.id,
        DocumentEntity.name,
        DocumentEntity.orderToken,
        TaskEntity.id,
        TaskEntity.documentId,
        TaskEntity.contentHtml,
        TaskEntity.orderToken,
        TaskEntity.indentation,
        TaskEntity.collapsed,
        TaskEntity.delayedUntil,
        TaskEntity.tags,
      )
    )
  def toNumber(field: ModelField.any): Int = fieldToNumberMap.get(field)
  def fromNumber(number: Int): ModelField.any = fieldToNumberMap.inverse().get(number)
}
