package app.flux.stores.document

import app.flux.stores.document.DocumentStore.State
import app.flux.stores.document.DocumentStore.SyncerWithReplenishingDelay
import app.models.document.Document
import app.models.document.DocumentEdit
import app.models.document.DocumentEdit.MaskedTaskUpdate
import app.models.document.DocumentEntity
import app.models.document.Task
import app.models.document.TaskEntity
import app.models.modification.EntityTypes
import hydro.common.Listenable
import hydro.common.Listenable.WritableListenable
import hydro.common.JsLoggingUtils.logExceptions
import hydro.common.time.Clock
import hydro.flux.stores.StateStore
import hydro.models.access.JsEntityAccess
import hydro.models.modification.EntityModification

import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.scalajs.js
import scala.scalajs.js.timers.SetTimeoutHandle

final class DocumentStore(initialDocument: Document)(implicit entityAccess: JsEntityAccess, clock: Clock)
    extends StateStore[State] {
  entityAccess.registerListener(JsEntityAccessListener)

  private var _state: State = State(
    document = initialDocument,
    pendingTaskIds = {
      val idsInDocument = initialDocument.tasks.map(_.id).toSet
      pendingModificationTaskIds(entityAccess) intersect idsInDocument
    },
  )
  private val syncer: SyncerWithReplenishingDelay[DocumentEdit.WithUpdateTimes] =
    new SyncerWithReplenishingDelay(
      delay = 500.milliseconds,
      emptyValue = DocumentEdit.WithUpdateTimes.empty,
      merge = _ mergedWith _,
      sync = syncDocumentEdit,
    )

  /**
    * Set that keeps the IDs of all added tasks.
    *
    * This is used to avoid UI glitches when a task is added and immediately removed.
    */
  private val alreadyAddedTaskIds: mutable.Set[Long] = mutable.Set()

  // **************** Implementation of StateStore methods **************** //
  override def state: State = _state

  // **************** Additional API **************** //
  /** Applies the given edit in the local state without calling the store listeners.
    *
    * Note that the listeners still will be called once the EntityModifications reach the back-end and are pushed back
    * to this front-end.
    */
  def applyEditWithoutCallingListeners(reversibleEdit: DocumentEdit.Reversible): Unit = {
    val editWithUpdateTimes =
      DocumentEdit.WithUpdateTimes.fromReversible(reversibleEdit)(clock, state.document)
    val newDocument = _state.document.withAppliedEdit(editWithUpdateTimes)
    val newPendingTaskIds = {
      val addedTaskIds = editWithUpdateTimes.addedTasks.map(_.id)
      val updatedTaskIds = editWithUpdateTimes.taskUpdatesById.keySet
      val removedTaskIds = editWithUpdateTimes.removedTasksIds
      _state.pendingTaskIds ++ addedTaskIds ++ updatedTaskIds -- removedTaskIds
    }
    _state = State(document = newDocument, pendingTaskIds = newPendingTaskIds)
    syncer.syncWithDelay(editWithUpdateTimes)
    alreadyAddedTaskIds ++= editWithUpdateTimes.addedTasks.map(_.id)
  }

  /** Number of task additions that is not yet synced to `EntityAccess`. */
  private[document] def unsyncedNumberOfTasks: Listenable[Int] =
    syncer.listenableValue.map { edit =>
      val involvedIds = Set() ++
        edit.addedTasks.map(_.id) ++
        edit.removedTasksIds ++
        edit.taskUpdatesById.keySet
      involvedIds.size
    }

  // **************** Private helper methods **************** //
  private def syncDocumentEdit(edit: DocumentEdit.WithUpdateTimes): Future[_] = {
    entityAccess.persistModifications(edit.toEntityModifications)
  }

  private def pendingModificationTaskIds(entityAccess: JsEntityAccess): Set[Long] = {
    entityAccess.pendingModifications.modifications
      .filter(_.entityType == TaskEntity.Type)
      .map(_.entityId)
      .toSet
  }

  // **************** Private inner types **************** //
  private[document] object JsEntityAccessListener extends JsEntityAccess.Listener {
    override def modificationsAddedOrPendingStateChanged(modifications: Seq[EntityModification]): Unit = {
      var newDocument = _state.document

      // Apply Task modifications
      newDocument = newDocument.withAppliedEdit(
        DocumentEdit.WithUpdateTimes.create(
          removedTasksIds = modifications.collect {
            case modification @ EntityModification.Remove(entityId)
                if modification.entityType == TaskEntity.Type =>
              entityId
          },
          addedTasks = modifications.collect {
            case EntityModification.Add(taskEntity: TaskEntity)
                if taskEntity.documentId == _state.document.id &&
                  !alreadyAddedTaskIds.contains(taskEntity.id) =>
              alreadyAddedTaskIds += taskEntity.id
              Task.fromTaskEntity(taskEntity)
          },
          taskUpdates = modifications.collect {
            case EntityModification.Update(taskEntity: TaskEntity)
                if taskEntity.documentId == _state.document.id =>
              Task.fromTaskEntity(taskEntity)
          },
        )
      )

      // Apply Document updates
      modifications.collect {
        case EntityModification.Update(documentEntity: DocumentEntity)
            if documentEntity.id == state.document.id =>
          newDocument = newDocument.updateFromDocumentEntity(documentEntity)
      }

      val newPendingTaskIds = {
        val changedTaskIds = modifications.map(_.entityId).toSet
        val changedStillPendingTaskIds = pendingModificationTaskIds(entityAccess) intersect changedTaskIds
        val noLongerPendingTaskIds = changedTaskIds -- changedStillPendingTaskIds

        // Add changedStillPendingTaskIds in case the pending tasks set needs to be extended because of this event
        _state.pendingTaskIds -- noLongerPendingTaskIds ++ changedStillPendingTaskIds
      }

      if (_state.document != newDocument || _state.pendingTaskIds != newPendingTaskIds) {
        _state = State(document = newDocument, pendingTaskIds = newPendingTaskIds)
        invokeStateUpdateListeners()
      }
    }
  }
}

object DocumentStore {
  case class State(document: Document, pendingTaskIds: Set[Long])

  private class SyncerWithReplenishingDelay[T](
      delay: FiniteDuration,
      emptyValue: T,
      merge: (T, T) => T,
      sync: T => Future[_],
  ) {
    private val currentValue: WritableListenable[T] = WritableListenable(emptyValue)
    private var timeoutHandle: SetTimeoutHandle = _

    def syncWithDelay(addedValue: T): Unit = {
      val newValue = merge(currentValue.get, addedValue)
      currentValue.set(newValue)

      if (timeoutHandle != null) {
        js.timers.clearTimeout(timeoutHandle)
      }
      timeoutHandle = js.timers.setTimeout(delay) {
        logExceptions {
          require(currentValue.get == newValue)
          currentValue.set(emptyValue)
          sync(newValue)
        }
      }
    }

    def listenableValue: Listenable[T] = currentValue
  }
}
