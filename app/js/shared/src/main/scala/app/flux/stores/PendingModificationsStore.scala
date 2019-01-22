package app.flux.stores

import app.flux.stores.PendingModificationsStore.State
import app.flux.stores.document.DocumentStoreFactory
import app.models.document.DocumentEntity
import app.models.document.TaskEntity
import hydro.models.modification.EntityModification
import app.models.user.User
import hydro.common.Listenable
import hydro.flux.stores.StateStore
import hydro.models.access.JsEntityAccess

import scala.collection.immutable.Seq

final class PendingModificationsStore(implicit jsEntityAccess: JsEntityAccess,
                                      documentStoreFactory: DocumentStoreFactory)
    extends StateStore[State] {
  jsEntityAccess.registerListener(JsEntityAccessListener)
  documentStoreFactory.unsyncedNumberOfTasks.registerListener(UnsyncedNumberOfDocumentTasksListener)

  private var _state: State = State(numberOfModifications = 0)

  // **************** Public API ****************//
  override def state: State = _state

  // **************** Private helper methods ****************//
  private def setState(state: State): Unit = {
    val originalState = _state
    _state = state
    if (_state != originalState) {
      invokeStateUpdateListeners()
    }
  }

  private def onAnyChange(): Unit = {
    val entityAccessCount =
      if (jsEntityAccess.pendingModifications.persistedLocally) {
        getModificationsSize(jsEntityAccess.pendingModifications.modifications)
      } else {
        0
      }
    val documentStoreCount = documentStoreFactory.unsyncedNumberOfTasks.get
    setState(State(numberOfModifications = entityAccessCount + documentStoreCount))
  }

  private def getModificationsSize(modifications: Seq[EntityModification]): Int = {
    var editCount = 0

    for (modification <- modifications) modification.entityType match {
      case User.Type           => editCount += 1
      case DocumentEntity.Type => editCount += 1
      case TaskEntity.Type     => // Do nothing (see below)
    }

    // Heuristic for counting TaskEntity modifications
    val taskEntityModifications = modifications.filter(_.entityType == TaskEntity.Type)
    val taskEntityAdditions = taskEntityModifications.filter(_.isInstanceOf[EntityModification.Add[_]])
    if (taskEntityAdditions.nonEmpty) editCount += taskEntityAdditions.size
    else editCount += taskEntityModifications.size

    editCount
  }

  // **************** Private inner types ****************//
  object JsEntityAccessListener extends JsEntityAccess.Listener {
    override def modificationsAddedOrPendingStateChanged(modifications: Seq[EntityModification]): Unit =
      onAnyChange()

    override def pendingModificationsPersistedLocally(): Unit = onAnyChange()
  }
  object UnsyncedNumberOfDocumentTasksListener extends Listenable.Listener[Int] {
    override def onChange(newValue: Int): Unit = onAnyChange()
  }
}
object PendingModificationsStore {

  /** numberOfModifications: Number of pending modifications that have been persisted locally. */
  case class State(numberOfModifications: Int)
}
