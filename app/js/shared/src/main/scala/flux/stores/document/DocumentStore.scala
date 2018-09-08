package flux.stores.document

import models.access.JsEntityAccess
import models.document.{Document, Task}
import models.modification.EntityModification

import scala.collection.immutable.Seq
import scala.collection.mutable

final class DocumentStore(initialDocument: Document)(implicit entityAccess: JsEntityAccess) {
  entityAccess.registerListener(JsEntityAccessListener)

  private var _document: Document = initialDocument

  def document: Document = _document

  def replaceTasks(toReplace: Iterable[Task], toAdd: Iterable[Task]): Document = {
    _document = _document.replaced(toReplace, toAdd)
    _document
  }

  private[document] object JsEntityAccessListener extends JsEntityAccess.Listener {
    override def modificationsAddedOrPendingStateChanged(modifications: Seq[EntityModification]): Unit = {
      // TODO
    }
  }
}
