package app.flux.stores.document

import app.flux.stores.document.DocumentStore.State
import app.flux.stores.document.DocumentStore.SyncerWithReplenishingDelay
import app.models.document.Document
import app.models.document.DocumentEdit
import app.models.document.DocumentEntity
import app.models.document.Task
import app.models.document.TaskEntity
import hydro.common.Listenable
import hydro.common.Listenable.WritableListenable
import hydro.common.LoggingUtils.logExceptions
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

  private var _state: State = State(document = initialDocument)
  private val syncer: SyncerWithReplenishingDelay[DocumentEdit] = new SyncerWithReplenishingDelay(
    delay = 500.milliseconds,
    emptyValue = DocumentEdit.empty,
    merge = _ mergedWith _,
    sync = syncDocumentEdit
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
  def applyEditWithoutCallingListeners(edit: DocumentEdit): Document = {
    val newDocument = _state.document.withAppliedEdit(edit)
    _state = _state.copy(document = newDocument)
    syncer.syncWithDelay(edit)
    alreadyAddedTaskIds ++= edit.addedTasks.map(_.id)
    newDocument
  }

  /** Number of task additions that is not yet synced to `EntityAccess`. */
  private[document] def unsyncedNumberOfTasks: Listenable[Int] =
    syncer.listenableValue.map { edit =>
      val involvedIds = Set() ++
        edit.addedTasks.map(_.id) ++
        edit.removedTasks.map(_.id) ++
        edit.taskUpdates.map(_.originalTask.id)
      involvedIds.size
    }

  // **************** Private helper methods **************** //
  private def syncDocumentEdit(edit: DocumentEdit): Future[_] = {
    entityAccess.persistModifications(edit.toEntityModifications)
  }

  // **************** Private inner types **************** //
  private[document] object JsEntityAccessListener extends JsEntityAccess.Listener {
    override def modificationsAddedOrPendingStateChanged(modifications: Seq[EntityModification]): Unit = {
      var newDocument = _state.document
      for (modification <- modifications) modification match {
        // Task updates
        case EntityModification.Add(taskEntity: TaskEntity)
            if taskEntity.documentId == _state.document.id && !alreadyAddedTaskIds.contains(taskEntity.id) =>
          alreadyAddedTaskIds += taskEntity.id
          newDocument = newDocument + Task.fromTaskEntity(taskEntity)
        case modification @ EntityModification.Remove(entityId)
            if modification.entityType == TaskEntity.Type =>
          newDocument = newDocument.minusTaskWithId(entityId)
        // Document updates
        case EntityModification.Update(documentEntity: DocumentEntity)
            if documentEntity.id == state.document.id =>
          newDocument = newDocument.updateFromDocumentEntity(documentEntity)

        case _ =>
      }

      if (_state.document != newDocument) {
        _state = _state.copy(document = newDocument)
        invokeStateUpdateListeners()
      }
    }
  }
}

object DocumentStore {
  case class State(document: Document)

  private class SyncerWithReplenishingDelay[T](delay: FiniteDuration,
                                               emptyValue: T,
                                               merge: (T, T) => T,
                                               sync: T => Future[_]) {
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
