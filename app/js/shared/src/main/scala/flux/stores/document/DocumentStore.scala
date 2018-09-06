package flux.stores.document

import models.access.JsEntityAccess
import models.document.Document
import models.modification.EntityModification

import scala.collection.immutable.Seq
import scala.collection.mutable

final class DocumentStore(initialDocument: Document)(implicit entityAccess: JsEntityAccess) {
  entityAccess.registerListener(JsEntityAccessListener)

  val document = initialDocument

  private[document] object JsEntityAccessListener extends JsEntityAccess.Listener {
    override def modificationsAddedOrPendingStateChanged(modifications: Seq[EntityModification]): Unit = {
      // TODO
    }
  }
}
