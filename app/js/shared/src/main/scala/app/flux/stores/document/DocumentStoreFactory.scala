package app.flux.stores.document

import hydro.common.LoggingUtils.logExceptions
import hydro.common.LoggingUtils.logFailure
import hydro.models.access.DbQueryImplicits._
import app.api.ScalaJsApi.GetInitialDataResponse
import hydro.common.Listenable
import hydro.common.Listenable.ListenableMap
import hydro.common.Listenable.WritableListenable
import hydro.flux.stores.AsyncEntityDerivedStateStore
import hydro.flux.stores.StateStore
import hydro.models.access.JsEntityAccess
import app.models.document.Document
import app.models.document.DocumentEntity
import app.models.modification.EntityModification
import app.models.modification.EntityType
import app.models.modification.EntityTypes
import app.models.document.TaskEntity
import app.models.document.DocumentEntity
import app.models.user.User

import scala.async.Async.async
import scala.async.Async.await
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

final class DocumentStoreFactory(implicit entityAccess: JsEntityAccess) {
  private val cache: ListenableMap[DocumentId, Future[DocumentStore]] = ListenableMap()

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

  /** Number of task additions that is not yet synced to `EntityAccess`. */
  def unsyncedNumberOfTasks: Listenable[Int] = {
    cache.flatMap { cacheMap =>
      val listenableInts: Iterable[Listenable[Int]] =
        for (storeFuture <- cacheMap.values)
          yield
            Listenable
              .fromFuture(storeFuture)
              .flatMap(storeOption =>
                if (storeOption.isDefined) storeOption.get.unsyncedNumberOfTasks else Listenable.fixed(0))
      listenableInts.toVector.reduceOption(Listenable.mergeWith[Int](_ + _)) getOrElse Listenable.fixed(0)
    }
  }

  // **************** private helper methods **************** //
  private def createNew(documentId: Long): Future[DocumentStore] = logFailure {
    async {
      val modificationsDuringCalculation = mutable.Buffer[EntityModification]()
      val entityModificationListener = new JsEntityAccess.Listener {
        override def modificationsAddedOrPendingStateChanged(modifications: Seq[EntityModification]): Unit = {
          modificationsDuringCalculation ++= modifications
        }
      }
      entityAccess.registerListener(entityModificationListener)

      val documentEntity = await(entityAccess.newQuery[DocumentEntity]().findById(documentId))
      val document = await(Document.fromDocumentEntity(documentEntity))
      val store = new DocumentStore(document)

      if (modificationsDuringCalculation.nonEmpty) {
        store.JsEntityAccessListener.modificationsAddedOrPendingStateChanged(
          modificationsDuringCalculation.toVector)
      }
      entityAccess.deregisterListener(entityModificationListener)
      store
    }
  }

  // **************** Type aliases ****************//
  private type DocumentId = Long
}
