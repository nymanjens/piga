package models.access

import common.GuavaReplacement.ImmutableBiMap
import common.OrderToken
import common.time.LocalDateTime
import models.Entity
import models.access.ModelField.FieldType
import models.document.{DocumentEntity, TaskEntity}
import models.modification.EntityType
import models.modification.EntityType._
import models.user.User

import scala.collection.immutable.Seq

/**
  * Represents a field in an model entity.
  *
  * @param name A name for this field that is unique in E
  * @tparam V The type of the values
  * @tparam E The type corresponding to the entity that contains this field
  */
sealed abstract class ModelField[V, E] private[access] (val name: String, accessor: E => V)(
    implicit val fieldType: FieldType[V]) {

  def get(entity: E): V = accessor(entity)
}

object ModelField {

  // **************** Methods **************** //
  def id[E <: Entity](implicit entityType: EntityType[E]): ModelField[Long, E] = entityType match {
    case UserType           => User.id.asInstanceOf[ModelField[Long, E]]
    case DocumentEntityType => DocumentEntity.id.asInstanceOf[ModelField[Long, E]]
    case TaskEntityType     => TaskEntity.id.asInstanceOf[ModelField[Long, E]]
  }

  // **************** Related types **************** //
  sealed trait FieldType[T]
  object FieldType {
    implicit case object BooleanType extends FieldType[Boolean]
    implicit case object IntType extends FieldType[Int]
    implicit case object LongType extends FieldType[Long]
    implicit case object DoubleType extends FieldType[Double]
    implicit case object StringType extends FieldType[String]
    implicit case object LocalDateTimeType extends FieldType[LocalDateTime]
    implicit case object MaybeLocalDateTimeType extends FieldType[Option[LocalDateTime]]
    implicit case object StringSeqType extends FieldType[Seq[String]]
    implicit case object OrderTokenType extends FieldType[OrderToken]
  }

  abstract sealed class IdModelField[E <: Entity] extends ModelField[Long, E]("id", _.idOption getOrElse -1)

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
    ImmutableBiMap
      .builder[ModelField[_, _], Int]()
      .put(User.id, 2)
      .put(User.loginName, 3)
      .put(User.passwordHash, 4)
      .put(User.name, 5)
      .put(User.isAdmin, 6)
      .put(DocumentEntity.id, 7)
      .put(DocumentEntity.name, 8)
      .put(DocumentEntity.orderToken, 14)
      .put(TaskEntity.id, 9)
      .put(TaskEntity.documentId, 10)
      .put(TaskEntity.contentHtml, 11)
      .put(TaskEntity.orderToken, 12)
      .put(TaskEntity.indentation, 13)
      .put(TaskEntity.collapsed, 15)
      .put(TaskEntity.delayedUntil, 16)
      .put(TaskEntity.tags, 17)
      .build()
  def toNumber(field: ModelField[_, _]): Int = fieldToNumberMap.get(field)
  def fromNumber(number: Int): ModelField[_, _] = fieldToNumberMap.inverse().get(number)
}
