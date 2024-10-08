package app.models.document

import app.models.document.DocumentEdit.MaskedTaskUpdate.FieldUpdate
import app.models.user.User
import hydro.common.OrderToken
import hydro.common.time.Clock
import hydro.common.time.LocalDateTime
import hydro.models.modification.EntityModification

import scala.collection.immutable.Seq

object DocumentEdit {
  case class Reversible(
      removedTasks: Seq[Task] = Seq(),
      addedTasks: Seq[Task] = Seq(),
      taskUpdates: Seq[MaskedTaskUpdate] = Seq(),
  ) {

    def reversed: DocumentEdit.Reversible = DocumentEdit.Reversible(
      removedTasks = addedTasks,
      addedTasks = removedTasks,
      taskUpdates = taskUpdates.map(_.reversed),
    )

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
            (removedTasks.sorted zip addedTasks.sorted).forall { case (t1, t2) =>
              t1 equalsIgnoringMetadata t2
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

  case class WithUpdateTimes private (
      removedTasksIds: Set[Long],
      addedTasks: Seq[Task],
      taskUpdatesById: Map[Long, Task],
  ) {

    def taskUpdates: Iterable[Task] = taskUpdatesById.values

    def mergedWith(that: DocumentEdit.WithUpdateTimes): DocumentEdit.WithUpdateTimes = {
      val overlappingTaskIds = this.addedTasks.map(_.id).toSet intersect that.removedTasksIds
      DocumentEdit.WithUpdateTimes(
        removedTasksIds = this.removedTasksIds ++ that.removedTasksIds.filterNot(overlappingTaskIds),
        addedTasks = this.addedTasks.filterNot(t => overlappingTaskIds contains t.id) ++ that.addedTasks,
        taskUpdatesById = {
          for ((id, updates) <- (this.taskUpdates ++ that.taskUpdates).groupBy(_.id))
            yield id -> updates.reduce(_ mergedWith _)
        },
      )
    }

    def toEntityModifications: Seq[EntityModification] = {
      val adds = addedTasks.map(t => EntityModification.Add(t.toTaskEntity))
      val updates = taskUpdates.map(t => EntityModification.Update(t.toTaskEntity))
      val deletes = removedTasksIds.map(id => EntityModification.Remove[TaskEntity](id))
      adds ++ updates ++ deletes
    }
  }
  object WithUpdateTimes {
    val empty =
      DocumentEdit.WithUpdateTimes(removedTasksIds = Set(), addedTasks = Seq(), taskUpdatesById = Map())

    def fromReversible(edit: DocumentEdit.Reversible)(implicit
        clock: Clock,
        document: Document,
    ): DocumentEdit.WithUpdateTimes =
      create(
        removedTasksIds = edit.removedTasks.map(_.id),
        addedTasks = edit.addedTasks,
        taskUpdates = for {
          update <- edit.taskUpdates
          if !update.isNoOp
          taskIndex <- document
            .maybeIndexOf(taskId = update.taskId, orderTokenHint = update.originalOrderToken)
        } yield document.tasks(taskIndex).withAppliedUpdateAndNewUpdateTime(update),
      )

    def create(
        removedTasksIds: Iterable[Long],
        addedTasks: Iterable[Task],
        taskUpdates: Iterable[Task],
    ): DocumentEdit.WithUpdateTimes =
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
      checked: Option[FieldUpdate[Boolean]],
      delayedUntil: Option[FieldUpdate[Option[LocalDateTime]]],
      tags: Option[FieldUpdate[Seq[String]]],
      lastContentModifierUserId: Option[FieldUpdate[Long]],
  ) {
    def reversed: MaskedTaskUpdate = MaskedTaskUpdate(
      taskId = taskId,
      originalOrderToken = orderToken.map(_.newValue) getOrElse originalOrderToken,
      content = content.map(_.reversed),
      orderToken = orderToken.map(_.reversed),
      indentation = indentation.map(_.reversed),
      collapsed = collapsed.map(_.reversed),
      checked = checked.map(_.reversed),
      delayedUntil = delayedUntil.map(_.reversed),
      tags = tags.map(_.reversed),
      lastContentModifierUserId = lastContentModifierUserId.map(_.reversed),
    )

    def isNoOp: Boolean = this.reversed == this

    def mergedWith(that: MaskedTaskUpdate): MaskedTaskUpdate = {
      def mergeFieldUpdates[V](
          thisFieldUpdate: Option[FieldUpdate[V]],
          thatFieldUpdate: Option[FieldUpdate[V]],
      ): Option[FieldUpdate[V]] =
        (thisFieldUpdate, thatFieldUpdate) match {
          case (None, None)    => None
          case (Some(u), None) => Some(u)
          case (None, Some(u)) => Some(u)
          case (Some(thisU), Some(thatU)) =>
            require(thisU.newValue == thatU.oldValue, "newValue and oldValue don't match")
            Some(FieldUpdate(oldValue = thisU.oldValue, newValue = thatU.newValue))
        }

      require(this.taskId == that.taskId, "Task IDs don't match")
      MaskedTaskUpdate(
        taskId = this.taskId,
        originalOrderToken = this.originalOrderToken,
        content = mergeFieldUpdates(this.content, that.content),
        orderToken = mergeFieldUpdates(this.orderToken, that.orderToken),
        indentation = mergeFieldUpdates(this.indentation, that.indentation),
        collapsed = mergeFieldUpdates(this.collapsed, that.collapsed),
        checked = mergeFieldUpdates(this.checked, that.checked),
        delayedUntil = mergeFieldUpdates(this.delayedUntil, that.delayedUntil),
        tags = mergeFieldUpdates(this.tags, that.tags),
        lastContentModifierUserId =
          mergeFieldUpdates(this.lastContentModifierUserId, that.lastContentModifierUserId),
      )
    }
  }
  object MaskedTaskUpdate {
    def fromFields(
        originalTask: Task,
        content: TextWithMarkup = null,
        orderToken: OrderToken = null,
        indentation: Int = -1,
        collapsed: java.lang.Boolean = null,
        checked: java.lang.Boolean = null,
        delayedUntil: Option[LocalDateTime] = null,
        tags: Seq[String] = null,
    )(implicit user: User): MaskedTaskUpdate = {
      def ifUpdate[V](value: Any, currentValue: V): Option[FieldUpdate[V]] = value match {
        case null | -1      => None
        case `currentValue` => None
        case _              => Some(FieldUpdate(oldValue = currentValue, newValue = value.asInstanceOf[V]))
      }
      val contentUpdate = ifUpdate(content, originalTask.content)
      MaskedTaskUpdate(
        taskId = originalTask.id,
        originalOrderToken = originalTask.orderToken,
        content = contentUpdate,
        orderToken = ifUpdate(orderToken, originalTask.orderToken),
        indentation = ifUpdate(indentation, originalTask.indentation),
        collapsed = ifUpdate(collapsed, originalTask.collapsed),
        checked = ifUpdate(checked, originalTask.checked),
        delayedUntil = ifUpdate(delayedUntil, originalTask.delayedUntil),
        tags = ifUpdate(tags, originalTask.tags),
        lastContentModifierUserId =
          if (contentUpdate.isDefined) ifUpdate(user.id, originalTask.lastContentModifierUserId)
          else None,
      )
    }

    case class FieldUpdate[V](oldValue: V, newValue: V) {
      def reversed: FieldUpdate[V] = FieldUpdate(oldValue = newValue, newValue = oldValue)

    }
  }
}
