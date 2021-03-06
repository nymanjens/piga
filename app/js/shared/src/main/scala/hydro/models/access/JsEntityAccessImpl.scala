package hydro.models.access

import app.api.ScalaJsApiClient
import hydro.models.modification.EntityModification
import hydro.models.modification.EntityType
import hydro.common.JsLoggingUtils.logExceptions
import hydro.common.Listenable
import hydro.common.Listenable.WritableListenable
import hydro.models.Entity
import hydro.models.access.JsEntityAccess.Listener

import scala.async.Async.async
import scala.async.Async.await
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

class JsEntityAccessImpl()(implicit
    apiClient: ScalaJsApiClient,
    remoteDatabaseProxy: RemoteDatabaseProxy,
    hydroPushSocketClientFactory: HydroPushSocketClientFactory,
) extends JsEntityAccess {

  private var listeners: Seq[Listener] = Seq()
  private var _pendingModifications: PendingModifications =
    PendingModifications(Seq(), persistedLocally = false)
  private var isCallingListeners: Boolean = false
  private val queryBlockingFutures: mutable.Buffer[Future[Unit]] = mutable.Buffer()
  private val _localDatabaseHasBeenLoaded: WritableListenable[Boolean] = WritableListenable(false)

  // Attach events to local database loading
  async {
    await(remoteDatabaseProxy.localDatabaseReadyFuture)
    val existingPendingModifications = await(remoteDatabaseProxy.pendingModifications())

    _pendingModifications = _pendingModifications.copy(persistedLocally = true)
    _pendingModifications ++= existingPendingModifications

    // Start listening to changes right after the existing list was loaded.
    // Note: There is a race condition here if updates happened between the fetching and the listening. Listening
    // too early has the risk of applying updates that are then overwritten (unless we hold on to them).
    remoteDatabaseProxy.registerPendingModificationsListener(UpdatingPendingModificationsListener)

    _localDatabaseHasBeenLoaded.set(true)

    // Heuristic: When the local database is also loaded and the pending modifications are loaded, pending
    // modifications will be stored or at least start being stored
    invokeListenersAsync(_.pendingModificationsPersistedLocally())

    // Optional: Try sending the existing pending modifications at application start, in the hope that the browser
    // is now online.
    if (existingPendingModifications.nonEmpty) {
      apiClient.persistEntityModifications(
        existingPendingModifications,
        waitUntilQueryReflectsModifications = false,
      )
    }

    // TODO: Move to SharedWorker
    // Send pending modifications whenever connection with the server is restored
    hydroPushSocketClientFactory.pushClientsAreOnline.registerListener { isOnline =>
      if (isOnline) {
        if (_pendingModifications.modifications.nonEmpty) {
          apiClient.persistEntityModifications(
            _pendingModifications.modifications,
            waitUntilQueryReflectsModifications = false,
          )
        }
      }
    }

  }

  // **************** Getters ****************//
  override def newQuery[E <: Entity: EntityType](): DbResultSet.Async[E] = {
    DbResultSet.fromExecutor(new DbQueryExecutor.Async[E] {
      override def data(dbQuery: DbQuery[E]) = async {
        if (queryBlockingFutures.nonEmpty) {
          await(queryBlockingFutures.last)
        }
        await(remoteDatabaseProxy.queryExecutor[E]().data(dbQuery))
      }
      override def count(dbQuery: DbQuery[E]) = async {
        if (queryBlockingFutures.nonEmpty) {
          await(queryBlockingFutures.last)
        }
        await(remoteDatabaseProxy.queryExecutor[E]().count(dbQuery))
      }
    })
  }

  override def pendingModifications = _pendingModifications

  override def localDatabaseHasBeenLoaded: Listenable[Boolean] = _localDatabaseHasBeenLoaded

  // **************** Setters ****************//
  override def persistModifications(modifications: Seq[EntityModification]): Future[Unit] = logExceptions {
    require(!isCallingListeners)

    _pendingModifications ++= modifications

    val persistResponse = remoteDatabaseProxy.persistEntityModifications(modifications)
    val listenersInvoked = persistResponse.queryReflectsModificationsFuture flatMap { _ =>
      invokeListenersAsync(_.modificationsAddedOrPendingStateChanged(modifications))
    }

    val queryBlockingFuture = persistResponse.queryReflectsModificationsFuture
    queryBlockingFutures += queryBlockingFuture
    queryBlockingFuture map { _ =>
      queryBlockingFutures -= queryBlockingFuture
    }

    async {
      await(persistResponse.completelyDoneFuture)
      await(listenersInvoked)
    }
  }

  override def clearLocalDatabase(): Future[Unit] = {
    remoteDatabaseProxy.clearLocalDatabase()
  }

  // **************** Other ****************//
  override def registerListener(listener: Listener): Unit = {
    require(!isCallingListeners)

    listeners = listeners :+ listener
  }

  override def deregisterListener(listener: Listener): Unit = {
    require(!isCallingListeners)

    listeners = listeners.filter(_ != listener)
  }

  // **************** Additional API ****************//
  def startCheckingForModifiedEntityUpdates(): Unit = {
    remoteDatabaseProxy.startCheckingForModifiedEntityUpdates(modifications => {
      _pendingModifications --= modifications
      invokeListenersAsync(_.modificationsAddedOrPendingStateChanged(modifications))
    })
  }

  // **************** Private helper methods ****************//
  private def invokeListenersAsync(func: Listener => Unit): Future[Unit] = {
    Future {
      logExceptions {
        require(!isCallingListeners)
        isCallingListeners = true
        listeners.foreach(func)
        isCallingListeners = false
      }
    }
  }

  // **************** Private inner types ****************//
  private object UpdatingPendingModificationsListener extends PendingModificationsListener {
    override def onPendingModificationAddedByOtherInstance(modification: EntityModification): Unit = {
      if (!(_pendingModifications.modifications contains modification)) {
        _pendingModifications ++= Seq(modification)
        invokeListenersAsync(_.modificationsAddedOrPendingStateChanged(Seq(modification)))
      }
    }
    override def onPendingModificationRemovedByOtherInstance(
        modificationPseudoUniqueIdentifier: Long
    ): Unit = {
      val modificationsToRemove =
        _pendingModifications.modifications.filter(
          _.pseudoUniqueIdentifier == modificationPseudoUniqueIdentifier
        )
      if (modificationsToRemove.nonEmpty) {
        _pendingModifications --= modificationsToRemove
        invokeListenersAsync(_.modificationsAddedOrPendingStateChanged(modificationsToRemove))
      }
    }
  }
}
