package flux.stores.document

import common.LoggingUtils.{logExceptions, logFailure}
import models.access.DbQueryImplicits._
import api.ScalaJsApi.GetInitialDataResponse
import flux.stores.{AsyncEntityDerivedStateStore, StateStore}
import models.access.JsEntityAccess
import models.document.{Document, DocumentEntity}
import models.modification.{EntityModification, EntityType}

import scala.async.Async.{async, await}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

final class DocumentStoreFactory(implicit entityAccess: JsEntityAccess) {
  private val cache: mutable.Map[DocumentId, Future[DocumentStore]] = mutable.Map()

  // **************** API ****************//
  def create(documentId: Long): Future[DocumentStore] = {
    if (cache contains documentId) {
      cache(documentId)
    } else {
      val created = createNew(documentId)
      cache.put(documentId, created)
      created
    }
  }

  // **************** private helper methods **************** //
  private def createNew(documentId: Long): Future[DocumentStore] = logFailure {
    async {
      val modificationsDuringCalculation = mutable.Buffer[EntityModification]()
      entityAccess.registerListener(new JsEntityAccess.Listener {
        override def modificationsAddedOrPendingStateChanged(modifications: Seq[EntityModification]): Unit = {
          modificationsDuringCalculation ++= modifications
        }
      })

      val documentEntity = await(entityAccess.newQuery[DocumentEntity]().findById(documentId))
      val document = await(Document.fromDocumentEntity(documentEntity))
      val store = new DocumentStore(document)

      if (modificationsDuringCalculation.nonEmpty) {
        store.JsEntityAccessListener.modificationsAddedOrPendingStateChanged(
          modificationsDuringCalculation.toVector)
      }
      store
    }
  }

  // **************** Type aliases ****************//
  private type DocumentId = Long
}
