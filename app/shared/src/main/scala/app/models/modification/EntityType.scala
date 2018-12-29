package app.models.modification

import app.common.ScalaUtils
import hydro.models.Entity
import app.models.document.DocumentEntity
import app.models.document.TaskEntity
import app.models.user.User

import scala.collection.immutable.Seq

/** Enumeration of all entity types that are transfered between server and client. */
sealed trait EntityType[E <: Entity] {
  type get = E

  def entityClass: Class[E]

  def checkRightType(entity: Entity): get = {
    require(
      entity.getClass == entityClass,
      s"Got entity of type ${entity.getClass}, but this entityType requires $entityClass")
    entity.asInstanceOf[E]
  }

  def name: String = ScalaUtils.objectName(this)
  override def toString = name
}
object EntityType {
  type any = EntityType[_ <: Entity]

  // @formatter:off
  implicit case object UserType extends EntityType[User] { override def entityClass = classOf[User]}
  implicit case object DocumentEntityType extends EntityType[DocumentEntity] { override def entityClass = classOf[DocumentEntity]}
  implicit case object TaskEntityType extends EntityType[TaskEntity] { override def entityClass = classOf[TaskEntity]}
  // @formatter:on

  val values: Seq[EntityType.any] = Seq(UserType, DocumentEntityType, TaskEntityType)
}
