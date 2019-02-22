package app.models.document

import app.models.access.ModelFields
import app.models.document.DocumentEdit.MaskedTaskUpdate
import hydro.common.OrderToken
import hydro.common.time.Clock
import hydro.common.time.LocalDateTime
import hydro.models.access.ModelField
import hydro.models.modification.EntityModification
import hydro.models.UpdatableEntity
import hydro.models.UpdatableEntity.LastUpdateTime

import scala.collection.immutable.Seq
import scala.collection.mutable

object DocumentEdit {
  case class Reversible(removedTasks: Seq[Task] = Seq(),
                        addedTasks: Seq[Task] = Seq(),
                        taskUpdates: Seq[MaskedTaskUpdate] = Seq()) {

    def reverse: DocumentEdit.Reversible = ???

    //  def toEntityModifications(implicit clock: Clock): Seq[EntityModification] = {
    //    val adds = addedTasks.map(t => EntityModification.Add(t.toTaskEntity))
    //    val deletes = removedTasks.map(t => EntityModification.createRemove(t.toTaskEntity))
    //    val updates = taskUpdates.map(_.toEntityModification)
    //  }

    def mergedWith(that: DocumentEdit.Reversible): DocumentEdit.Reversible = {
      val overlappingTasks = this.addedTasks.toSet intersect that.removedTasks.toSet
      DocumentEdit.Reversible(
        removedTasks = this.removedTasks ++ that.removedTasks.filterNot(overlappingTasks),
        addedTasks = that.addedTasks ++ this.addedTasks.filterNot(overlappingTasks),
        taskUpdates = ???
      )
    }

    def isNoOp: Boolean = {
      def removedEqualsAdded = {
        if (removedTasks.size == addedTasks.size) {
          if (removedTasks.isEmpty) {
            true
          } else {
            (removedTasks.sorted zip addedTasks.sorted).forall {
              case (t1, t2) => t1 equalsIgnoringMetadata t2
            }
          }
        } else {
          false
        }
      }
      def updatesAreNoOp = taskUpdates.forall(_.isNoOp)

      removedEqualsAdded && updatesAreNoOp
    }
  }

  case class WithUpdateTimes(removedTasksIds: Set[Long],
                             addedTasks: Seq[Task],
                             taskUpdatesById: Map[Long, Task]) {

    def taskUpdates: Iterable[Task] = taskUpdatesById.values

    def mergedWith(that: DocumentEdit.WithUpdateTimes): DocumentEdit.WithUpdateTimes = ???

    def toEntityModifications: Seq[EntityModification] = ???
  }
  object WithUpdateTimes {
    val empty =
      DocumentEdit.WithUpdateTimes(removedTasksIds = Set(), addedTasks = Seq(), taskUpdatesById = Map())

    def fromReversible(edit: DocumentEdit.Reversible)(implicit clock: Clock): DocumentEdit.WithUpdateTimes =
      ???
  }

  case class MaskedTaskUpdate private (
      originalTask: Task,
      content: Option[TextWithMarkup],
      orderToken: Option[OrderToken],
      indentation: Option[Int],
      collapsed: Option[Boolean],
      delayedUntil: Option[Option[LocalDateTime]],
      tags: Option[Seq[String]],
  ) {
    def reverse: MaskedTaskUpdate = ???

    def isNoOp: Boolean = ???

    def toEntityModification(implicit clock: Clock): EntityModification.Update[TaskEntity] = {
      var newTaskEntity = originalTask.toTaskEntity
      val fieldMask = mutable.Buffer[ModelField[_, TaskEntity]]()

      def maybeApplyField[V](maybeValue: Option[V], field: ModelField[V, TaskEntity]): Unit = {
        if (maybeValue.isDefined) {
          newTaskEntity = field.set(newTaskEntity, maybeValue.get)
          fieldMask.append(field)
        }
      }

      maybeApplyField(content.map(_.toHtml), ModelFields.TaskEntity.contentHtml)
      maybeApplyField(orderToken, ModelFields.TaskEntity.orderToken)
      maybeApplyField(indentation, ModelFields.TaskEntity.indentation)
      maybeApplyField(collapsed, ModelFields.TaskEntity.collapsed)
      maybeApplyField(delayedUntil, ModelFields.TaskEntity.delayedUntil)
      maybeApplyField(tags, ModelFields.TaskEntity.tags)

      EntityModification.createUpdate(newTaskEntity, fieldMask.toVector)
    }
  }
  object MaskedTaskUpdate {
    def fromFields(
        originalTask: Task,
        content: TextWithMarkup = null,
        orderToken: OrderToken = null,
        indentation: Int = -1,
        collapsed: java.lang.Boolean = null,
        delayedUntil: Option[LocalDateTime] = null,
        tags: Seq[String] = null,
    ): MaskedTaskUpdate = {
      def ifUpdate[V](value: V, currentValue: V): Option[V] = value match {
        case null | -1      => None
        case `currentValue` => None
        case _              => Some(value)
      }
      MaskedTaskUpdate(
        originalTask = originalTask,
        content = ifUpdate(content, originalTask.content),
        orderToken = ifUpdate(orderToken, originalTask.orderToken),
        indentation = ifUpdate(indentation, originalTask.indentation),
        collapsed = ifUpdate(collapsed, originalTask.collapsed),
        delayedUntil = ifUpdate(delayedUntil, originalTask.delayedUntil),
        tags = ifUpdate(tags, originalTask.tags),
      )
    }
  }
}
