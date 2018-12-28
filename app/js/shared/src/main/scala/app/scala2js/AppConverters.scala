package app.scala2js

import app.models._
import app.models.access.ModelField
import app.models.document.DocumentEntity
import app.models.document.TaskEntity
import app.models.modification._
import app.models.user.User
import hydro.scala2js.Scala2Js.Converter
import hydro.scala2js.Scala2Js.MapConverter
import hydro.scala2js.StandardConverters
import hydro.scala2js.StandardConverters.EntityConverter

import scala.collection.immutable.Seq

object AppConverters {

  // **************** Convertor generators **************** //
  implicit def fromEntityType[E <: Entity: EntityType]: MapConverter[E] = {
    val entityType: EntityType[E] = implicitly[EntityType[E]]
    val converter: MapConverter[_ <: Entity] = entityType match {
      case EntityType.UserType           => UserConverter
      case EntityType.DocumentEntityType => DocumentEntityConverter
      case EntityType.TaskEntityType     => TaskEntityConverter
    }
    converter.asInstanceOf[MapConverter[E]]
  }

  // **************** General converters **************** //
  implicit val EntityTypeConverter: Converter[EntityType.any] =
    StandardConverters.enumConverter(
      EntityType.UserType,
      EntityType.DocumentEntityType,
      EntityType.TaskEntityType)

  // **************** Entity converters **************** //
  implicit val UserConverter: EntityConverter[User] = new EntityConverter(
    allFieldsWithoutId = Seq(
      ModelField.User.loginName,
      ModelField.User.passwordHash,
      ModelField.User.name,
      ModelField.User.isAdmin,
    ),
    toScalaWithoutId = dict =>
      User(
        loginName = dict.getRequired(ModelField.User.loginName),
        passwordHash = dict.getRequired(ModelField.User.passwordHash),
        name = dict.getRequired(ModelField.User.name),
        isAdmin = dict.getRequired(ModelField.User.isAdmin)
    )
  )

  implicit val DocumentEntityConverter: EntityConverter[DocumentEntity] = new EntityConverter(
    allFieldsWithoutId = Seq(
      ModelField.DocumentEntity.name,
      ModelField.DocumentEntity.orderToken,
    ),
    toScalaWithoutId = dict =>
      DocumentEntity(
        name = dict.getRequired(ModelField.DocumentEntity.name),
        orderToken = dict.getRequired(ModelField.DocumentEntity.orderToken))
  )

  implicit val TaskEntityConverter: EntityConverter[TaskEntity] = new EntityConverter(
    allFieldsWithoutId = Seq(
      ModelField.TaskEntity.documentId,
      ModelField.TaskEntity.contentHtml,
      ModelField.TaskEntity.orderToken,
      ModelField.TaskEntity.indentation,
      ModelField.TaskEntity.collapsed,
      ModelField.TaskEntity.delayedUntil,
      ModelField.TaskEntity.tags,
    ),
    toScalaWithoutId = dict =>
      TaskEntity(
        documentId = dict.getRequired(ModelField.TaskEntity.documentId),
        contentHtml = dict.getRequired(ModelField.TaskEntity.contentHtml),
        orderToken = dict.getRequired(ModelField.TaskEntity.orderToken),
        indentation = dict.getRequired(ModelField.TaskEntity.indentation),
        collapsed = dict.getRequired(ModelField.TaskEntity.collapsed),
        delayedUntil = dict.getRequired(ModelField.TaskEntity.delayedUntil),
        tags = dict.getRequired(ModelField.TaskEntity.tags)
    )
  )
}
