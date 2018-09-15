package flux.stores.document

import common.LoggingUtils.{logExceptions, logFailure}
import models.access.DbQueryImplicits._
import api.ScalaJsApi.GetInitialDataResponse
import common.Listenable
import common.Listenable.{ListenableMap, WritableListenable}
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
