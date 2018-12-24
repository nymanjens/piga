package flux.stores.document

import common.LoggingUtils.LogExceptionsCallback
import common.LoggingUtils.logExceptions
import common.Listenable
import common.Listenable.WritableListenable

import scala.concurrent.duration._
import scala.scalajs.js
import hydro.flux.stores.StateStore
import flux.stores.document.DocumentStore.Replacement
import flux.stores.document.DocumentStore.State
import flux.stores.document.DocumentStore.SyncerWithReplenishingDelay
import models.access.JsEntityAccess
import models.document.Document
import models.document.DocumentEntity
import models.document.Task
import models.document.TaskEntity
import models.modification.EntityModification
import models.modification.EntityType

import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.concurrent.Future
import scala.scalajs.js.timers.SetTimeoutHandle

final class DocumentStore(initialDocument: Document)(implicit entityAccess: JsEntityAccess)
    extends StateStore[State] {
  entityAccess.registerListener(JsEntityAccessListener)

  private var _state: State = State(document = initialDocument)
  private val syncer: SyncerWithReplenishingDelay[Replacement] = new SyncerWithReplenishingDelay(
    delay = 500.milliseconds,
    emptyValue = Replacement.empty,
    merge = _ merge _,
    sync = syncReplacement
  )
  private val alreadyAddedTaskIds: mutable.Set[Long] = mutable.Set()

  // **************** Implementation of StateStore methods **************** //
  override def state: State = _state

  // **************** Additional API **************** //
  /** Replaces tasks in state without calling the store listeners.
    *
    * Note that the listeners still will be called once the EntityModifications reach the back-end and are pushed back
    * to this front-end.
    */
  def replaceTasksWithoutCallingListeners(toReplace: Iterable[Task], toAdd: Iterable[Task]): Document = {
    val newDocument = _state.document.replaced(toReplace, toAdd)
    _state = _state.copy(document = newDocument)
    syncer.syncWithDelay(Replacement(removedTasks = toReplace.toSet, addedTasks = toAdd.toSet))
    alreadyAddedTaskIds ++= toAdd.map(_.id)
    newDocument
  }

  /** Number of task additions that is not yet synced to `EntityAccess`. */
  private[document] def unsyncedNumberOfTasks: Listenable[Int] =
    syncer.listenableValue.map(_.addedTasks.size)

  // **************** Private helper methods **************** //
  private def syncReplacement(replacement: Replacement): Future[_] = {
    val deletes = replacement.removedTasks.toVector
      .map(t => EntityModification.createDelete(t.toTaskEntity(_state.document)))
    val adds = replacement.addedTasks.map(t => EntityModification.Add(t.toTaskEntity(_state.document)))

    entityAccess.persistModifications(deletes ++ adds)
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
            if modification.entityType == EntityType.TaskEntityType =>
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

  private case class Replacement(removedTasks: Set[Task], addedTasks: Set[Task]) {
    def merge(that: Replacement): Replacement = {
      val overlappingTasks = this.addedTasks intersect that.removedTasks

      Replacement(
        removedTasks = this.removedTasks ++ that.removedTasks.filterNot(overlappingTasks),
        addedTasks = that.addedTasks ++ this.addedTasks.filterNot(overlappingTasks),
      )
    }
  }
  private object Replacement {
    val empty: Replacement = Replacement(Set(), Set())
  }
}
