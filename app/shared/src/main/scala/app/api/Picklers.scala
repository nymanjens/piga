package app.api

import java.time.LocalDate
import java.time.LocalTime

import boopickle.Default._
import hydro.common.time.LocalDateTime
import app.models.Entity
import app.models.access.ModelFields
import hydro.models.access.ModelField
import app.models.document.DocumentEntity
import app.models.document.TaskEntity
import app.models.modification.EntityModification
import app.models.modification.EntityType
import app.models.modification.EntityType._
import app.models.Entity
import app.models.modification.EntityModification
import app.models.modification.EntityType
import app.models.modification.EntityType._
import app.models.user.User
import boopickle.Default._
import hydro.api.StandardPicklers

import scala.collection.immutable.Seq
import scala.collection.immutable.Set

object Picklers extends StandardPicklers {

  // Pickler that does the same as an autogenerated User pickler, except that it redacts the user's password
  implicit object UserPickler extends Pickler[User] {
    override def pickle(user: User)(implicit state: PickleState): Unit = logExceptions {
      state.pickle(user.loginName)
      // Password redacted
      state.pickle(user.name)
      state.pickle(user.isAdmin)
      state.pickle(user.idOption)
    }
    override def unpickle(implicit state: UnpickleState): User = logExceptions {
      User(
        loginName = state.unpickle[String],
        passwordHash = "<redacted>",
        name = state.unpickle[String],
        isAdmin = state.unpickle[Boolean],
        idOption = state.unpickle[Option[Long]]
      )
    }
  }

  implicit object EntityTypePickler extends Pickler[EntityType.any] {
    override def pickle(entityType: EntityType.any)(implicit state: PickleState): Unit = logExceptions {
      val intValue: Int = entityType match {
        case UserType           => 1
        case DocumentEntityType => 2
        case TaskEntityType     => 3
      }
      state.pickle(intValue)
    }
    override def unpickle(implicit state: UnpickleState): EntityType.any = logExceptions {
      state.unpickle[Int] match {
        case 1 => UserType
        case 2 => DocumentEntityType
        case 3 => TaskEntityType
      }
    }
  }

  implicit val entityPickler = compositePickler[Entity]
    .addConcreteType[User]
    .addConcreteType[DocumentEntity]
    .addConcreteType[TaskEntity]

  implicit object EntityModificationPickler extends Pickler[EntityModification] {
    val addNumber = 1
    val updateNumber = 3
    val removeNumber = 2

    override def pickle(modification: EntityModification)(implicit state: PickleState): Unit =
      logExceptions {
        state.pickle[EntityType.any](modification.entityType)
        // Pickle number
        state.pickle(modification match {
          case _: EntityModification.Add[_]    => addNumber
          case _: EntityModification.Update[_] => updateNumber
          case _: EntityModification.Remove[_] => removeNumber
        })
        modification match {
          case EntityModification.Add(entity)      => state.pickle(entity)
          case EntityModification.Update(entity)   => state.pickle(entity)
          case EntityModification.Remove(entityId) => state.pickle(entityId)
        }
      }
    override def unpickle(implicit state: UnpickleState): EntityModification = logExceptions {
      val entityType = state.unpickle[EntityType.any]
      state.unpickle[Int] match {
        case `addNumber` =>
          val entity = state.unpickle[Entity]
          def addModification[E <: Entity](entity: Entity, entityType: EntityType[E]): EntityModification = {
            EntityModification.Add(entityType.checkRightType(entity))(entityType)
          }
          addModification(entity, entityType)
        case `updateNumber` =>
          val entity = state.unpickle[Entity]
          def updateModification[E <: Entity](entity: Entity,
                                              entityType: EntityType[E]): EntityModification = {
            EntityModification.Update(entityType.checkRightType(entity))(entityType)
          }
          updateModification(entity, entityType)
        case `removeNumber` =>
          val entityId = state.unpickle[Long]
          EntityModification.Remove(entityId)(entityType)
      }
    }
  }
}
