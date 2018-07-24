package flux.stores

import flux.stores.PendingModificationsStore.State
import models.access.JsEntityAccess
import models.modification.{EntityModification, EntityType}

import scala.collection.immutable.Seq

final class PendingModificationsStore(implicit jsEntityAccess: JsEntityAccess) extends StateStore[State] {
  jsEntityAccess.registerListener(JsEntityAccessListener)

  private var _state: State = State(numberOfModifications = 0)

  // **************** Public API ****************//
  override def state: State = _state

  // **************** Private state helper methods ****************//
  private def setState(state: State): Unit = {
    val originalState = _state
    _state = state
    if (_state != originalState) {
      invokeStateUpdateListeners()
    }
  }

  // **************** Private inner types ****************//
  object JsEntityAccessListener extends JsEntityAccess.Listener {
    override def modificationsAddedOrPendingStateChanged(modifications: Seq[EntityModification]): Unit =
      onAnyChange()

    override def pendingModificationsPersistedLocally(): Unit = onAnyChange()

    private def onAnyChange(): Unit = {
      if (jsEntityAccess.pendingModifications.persistedLocally) {
        setState(
          State(
            numberOfModifications = getModificationsSize(jsEntityAccess.pendingModifications.modifications)))
      } else {
        setState(State(numberOfModifications = 0))
      }
    }

    private def getModificationsSize(modifications: Seq[EntityModification]): Int = {
      // TODO
      var editCount = 0

      for (modification <- modifications) modification.entityType match {
        case EntityType.UserType => editCount += 1
      }

      editCount
    }
  }
}
object PendingModificationsStore {

  /** numberOfModifications: Number of pending modifications that have been persisted locally. */
  case class State(numberOfModifications: Int)
}