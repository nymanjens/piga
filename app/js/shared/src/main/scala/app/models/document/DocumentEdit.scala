package app.models.document

import app.models.access.ModelFields
import app.models.document.DocumentEdit.MaskedTaskUpdate
import app.models.document.DocumentEdit.MaskedTaskUpdate.FieldUpdate
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

    def reversed: DocumentEdit.Reversible = DocumentEdit.Reversible(
      removedTasks = addedTasks,
      addedTasks = removedTasks,
      taskUpdates = taskUpdates.map(_.reversed),
    )

    // TODO: Remove
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
        taskUpdates = {
          val idToUpdates = (this.taskUpdates ++ that.taskUpdates).groupBy(_.taskId)
          idToUpdates.values.map(_.reduce(_ mergedWith _)).toVector
        },
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

  case class WithUpdateTimes private (removedTasksIds: Set[Long],
                                      addedTasks: Seq[Task],
                                      taskUpdatesById: Map[Long, Task]) {

    def taskUpdates: Iterable[Task] = taskUpdatesById.values

    def mergedWith(that: DocumentEdit.WithUpdateTimes): DocumentEdit.WithUpdateTimes = ???

    def toEntityModifications: Seq[EntityModification] = ???
  }
  object WithUpdateTimes {
    val empty =
      DocumentEdit.WithUpdateTimes(removedTasksIds = Set(), addedTasks = Seq(), taskUpdatesById = Map())

    def fromReversible(edit: DocumentEdit.Reversible)(implicit clock: Clock,
                                                      document: Document): DocumentEdit.WithUpdateTimes =
      create(
        removedTasksIds = edit.removedTasks.map(_.id),
        addedTasks = edit.addedTasks,
        taskUpdates = edit.taskUpdates
          .flatMap { update =>
            document
              .taskOption(id = update.taskId, orderTokenHint = update.originalOrderToken)
              .map(currentTask => currentTask.withAppliedUpdateAndNewUpdateTime(update))
          },
      )

    def create(removedTasksIds: Iterable[Long],
               addedTasks: Iterable[Task],
               taskUpdates: Iterable[Task]): DocumentEdit.WithUpdateTimes =
      WithUpdateTimes(
        removedTasksIds = removedTasksIds.toSet,
        addedTasks = addedTasks.toVector,
        taskUpdatesById = taskUpdates.map(u => u.id -> u).toMap,
      )
  }

  case class MaskedTaskUpdate private (
      taskId: Long,
      originalOrderToken: OrderToken,
      content: Option[FieldUpdate[TextWithMarkup]],
      orderToken: Option[FieldUpdate[OrderToken]],
      indentation: Option[FieldUpdate[Int]],
      collapsed: Option[FieldUpdate[Boolean]],
      delayedUntil: Option[FieldUpdate[Option[LocalDateTime]]],
      tags: Option[FieldUpdate[Seq[String]]],
  ) {
    def reversed: MaskedTaskUpdate = MaskedTaskUpdate(
      taskId = taskId,
      originalOrderToken = orderToken.map(_.newValue) getOrElse originalOrderToken,
      content = content.map(_.reversed),
      orderToken = orderToken.map(_.reversed),
      indentation = indentation.map(_.reversed),
      collapsed = collapsed.map(_.reversed),
      delayedUntil = delayedUntil.map(_.reversed),
      tags = tags.map(_.reversed),
    )

    def isNoOp: Boolean = ???

    def mergedWith(that: MaskedTaskUpdate): MaskedTaskUpdate = ???

    // TODO: Remove
//    def toEntityModification(implicit clock: Clock): EntityModification.Update[TaskEntity] = {
//      var newTaskEntity = originalTask.toTaskEntity
//      val fieldMask = mutable.Buffer[ModelField[_, TaskEntity]]()
//
//      def maybeApplyField[V](maybeValue: Option[V], field: ModelField[V, TaskEntity]): Unit = {
//        if (maybeValue.isDefined) {
//          newTaskEntity = field.set(newTaskEntity, maybeValue.get)
//          fieldMask.append(field)
//        }
//      }
//
//      maybeApplyField(content.map(_.toHtml), ModelFields.TaskEntity.contentHtml)
//      maybeApplyField(orderToken, ModelFields.TaskEntity.orderToken)
//      maybeApplyField(indentation, ModelFields.TaskEntity.indentation)
//      maybeApplyField(collapsed, ModelFields.TaskEntity.collapsed)
//      maybeApplyField(delayedUntil, ModelFields.TaskEntity.delayedUntil)
//      maybeApplyField(tags, ModelFields.TaskEntity.tags)
//
//      EntityModification.createUpdate(newTaskEntity, fieldMask.toVector)
//    }
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
      def ifUpdate[V](value: V, currentValue: V): Option[FieldUpdate[V]] = value match {
        case null | -1      => None
        case `currentValue` => None
        case _              => Some(FieldUpdate(oldValue = currentValue, newValue = value))
      }
      MaskedTaskUpdate(
        taskId = originalTask.id,
        originalOrderToken = originalTask.orderToken,
        content = ifUpdate(content, originalTask.content),
        orderToken = ifUpdate(orderToken, originalTask.orderToken),
        indentation = ifUpdate(indentation, originalTask.indentation),
        collapsed = ifUpdate(collapsed, originalTask.collapsed),
        delayedUntil = ifUpdate(delayedUntil, originalTask.delayedUntil),
        tags = ifUpdate(tags, originalTask.tags),
      )
    }

    case class FieldUpdate[V](oldValue: V, newValue: V) {
      def reversed: FieldUpdate[V] = FieldUpdate(oldValue = newValue, newValue = oldValue)

    }
  }
}
