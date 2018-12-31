package app.models.access

import app.common.GuavaReplacement.ImmutableBiMap
import app.common.OrderToken
import app.models.document.DocumentEntity
import app.models.document.TaskEntity
import app.models.modification.EntityType
import app.models.user.User
import hydro.common.time.LocalDateTime
import hydro.models.Entity
import hydro.models.access.ModelField
import hydro.models.access.ModelField.IdModelField
import hydro.models.access.ModelField.toBiMapWithUniqueValues

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
    case object loginName extends ModelField[String, E]("loginName", _.loginName)
    case object passwordHash extends ModelField[String, E]("passwordHash", _.passwordHash)
    case object name extends ModelField[String, E]("name", _.name)
    case object isAdmin extends ModelField[Boolean, E]("isAdmin", _.isAdmin)
  }

  object DocumentEntity {
    private type E = DocumentEntity

    case object id extends IdModelField[E]
    case object name extends ModelField[String, E]("name", _.name)
    case object orderToken extends ModelField[OrderToken, E]("orderToken", _.orderToken)
  }

  object TaskEntity {
    private type E = TaskEntity

    case object id extends IdModelField[E]
    case object documentId extends ModelField[Long, E]("documentId", _.documentId)
    case object contentHtml extends ModelField[String, E]("contentHtml", _.contentHtml)
    case object orderToken extends ModelField[OrderToken, E]("orderToken", _.orderToken)
    case object indentation extends ModelField[Int, E]("indentation", _.indentation)
    case object collapsed extends ModelField[Boolean, E]("collapsed", _.collapsed)
    case object delayedUntil extends ModelField[Option[LocalDateTime], E]("delayedUntil", _.delayedUntil)
    case object tags extends ModelField[Seq[String], E]("tags", _.tags)
  }

  // **************** Field numbers **************** //
  private val fieldToNumberMap: ImmutableBiMap[ModelField[_, _], Int] =
    toBiMapWithUniqueValues(
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
  def toNumber(field: ModelField[_, _]): Int = fieldToNumberMap.get(field)
  def fromNumber(number: Int): ModelField[_, _] = fieldToNumberMap.inverse().get(number)
}
