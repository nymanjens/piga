package app.scala2js

import app.models.access.ModelFields
import app.models.document.DocumentEntity
import app.models.document.DocumentPermissionAndPlacement
import app.models.document.TaskEntity
import app.models.user.User
import hydro.models.modification.EntityType
import hydro.models.Entity
import hydro.scala2js.Scala2Js.MapConverter
import hydro.scala2js.StandardConverters.EntityConverter

import scala.collection.immutable.Seq

object AppConverters {

  // **************** Convertor generators **************** //
  implicit def fromEntityType[E <: Entity: EntityType]: MapConverter[E] = {
    val entityType: EntityType[E] = implicitly[EntityType[E]]
    val converter: MapConverter[_ <: Entity] = entityType match {
      case User.Type                           => UserConverter
      case DocumentEntity.Type                 => DocumentEntityConverter
      case DocumentPermissionAndPlacement.Type => DocumentPermissionAndPlacementConverter
      case TaskEntity.Type                     => TaskEntityConverter
    }
    converter.asInstanceOf[MapConverter[E]]
  }

  // **************** Entity converters **************** //
  implicit val UserConverter: EntityConverter[User] = new EntityConverter(
    allFieldsWithoutId = Seq(
      ModelFields.User.loginName,
      ModelFields.User.passwordHash,
      ModelFields.User.name,
      ModelFields.User.isAdmin,
    ),
    toScalaWithoutId = dict =>
      User(
        loginName = dict.getRequired(ModelFields.User.loginName),
        passwordHash = dict.getRequired(ModelFields.User.passwordHash),
        name = dict.getRequired(ModelFields.User.name),
        isAdmin = dict.getRequired(ModelFields.User.isAdmin),
      ),
  )

  implicit val DocumentEntityConverter: EntityConverter[DocumentEntity] = new EntityConverter(
    allFieldsWithoutId = Seq(
      ModelFields.DocumentEntity.name
    ),
    toScalaWithoutId = dict =>
      DocumentEntity(
        name = dict.getRequired(ModelFields.DocumentEntity.name)
      ),
  )

  implicit val DocumentPermissionAndPlacementConverter: EntityConverter[DocumentPermissionAndPlacement] =
    new EntityConverter(
      allFieldsWithoutId = Seq(
        ModelFields.DocumentPermissionAndPlacement.documentId,
        ModelFields.DocumentPermissionAndPlacement.userId,
        ModelFields.DocumentPermissionAndPlacement.orderToken,
      ),
      toScalaWithoutId = dict =>
        DocumentPermissionAndPlacement(
          documentId = dict.getRequired(ModelFields.DocumentPermissionAndPlacement.documentId),
          userId = dict.getRequired(ModelFields.DocumentPermissionAndPlacement.userId),
          orderToken = dict.getRequired(ModelFields.DocumentPermissionAndPlacement.orderToken),
        ),
    )

  implicit val TaskEntityConverter: EntityConverter[TaskEntity] = new EntityConverter(
    allFieldsWithoutId = Seq(
      ModelFields.TaskEntity.documentId,
      ModelFields.TaskEntity.contentHtml,
      ModelFields.TaskEntity.orderToken,
      ModelFields.TaskEntity.indentation,
      ModelFields.TaskEntity.collapsed,
      ModelFields.TaskEntity.delayedUntil,
      ModelFields.TaskEntity.tags,
      ModelFields.TaskEntity.lastContentModifierUserId,
    ),
    toScalaWithoutId = dict =>
      TaskEntity(
        documentId = dict.getRequired(ModelFields.TaskEntity.documentId),
        contentHtml = dict.getRequired(ModelFields.TaskEntity.contentHtml),
        orderToken = dict.getRequired(ModelFields.TaskEntity.orderToken),
        indentation = dict.getRequired(ModelFields.TaskEntity.indentation),
        collapsed = dict.getRequired(ModelFields.TaskEntity.collapsed),
        delayedUntil = dict.getRequired(ModelFields.TaskEntity.delayedUntil),
        tags = dict.getRequired(ModelFields.TaskEntity.tags),
        lastContentModifierUserId = dict.getRequired(ModelFields.TaskEntity.lastContentModifierUserId),
      ),
  )
}
