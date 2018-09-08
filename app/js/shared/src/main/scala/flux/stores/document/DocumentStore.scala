package flux.stores.document

import flux.stores.StateStore
import flux.stores.document.DocumentStore.State
import models.access.JsEntityAccess
import models.document.{Document, Task}
import models.modification.EntityModification

import scala.collection.immutable.Seq
import scala.collection.mutable

final class DocumentStore(initialDocument: Document)(implicit entityAccess: JsEntityAccess)
    extends StateStore[State] {
  entityAccess.registerListener(JsEntityAccessListener)

  private var _state: State = State(document = initialDocument)

  // **************** Implementation of StateStore methods **************** //
  override def state: State = _state

  // **************** Additional public API **************** //
  /** Replaces tasks in state without calling the store listeners.
    *
    * Note that the listeners still will be called once the EntityModifications reach the back-end and are pushed back
    * to this front-end.
    */
  def replaceTasksWithoutCallingListeners(toReplace: Iterable[Task], toAdd: Iterable[Task]): Document = {
    val newDocument = _state.document.replaced(toReplace, toAdd)
    _state = _state.copy(document = newDocument)
    newDocument
  }

  private[document] object JsEntityAccessListener extends JsEntityAccess.Listener {
    override def modificationsAddedOrPendingStateChanged(modifications: Seq[EntityModification]): Unit = {
      // TODO
    }
  }
}

object DocumentStore {
  case class State(document: Document)
}
