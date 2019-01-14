package app.flux.stores.document

import app.api.ScalaJsApi.GetInitialDataResponse
import app.api.ScalaJsApiClient
import hydro.common.OrderToken
import app.flux.action.AppActions._
import app.flux.stores.document.AllDocumentsStore.State
import app.models.document.DocumentEntity
import app.models.document.TaskEntity
import hydro.models.modification.EntityModification
import hydro.flux.action.Dispatcher
import hydro.flux.stores.AsyncEntityDerivedStateStore
import hydro.flux.stores.StateStore
import hydro.models.access.JsEntityAccess

import scala.async.Async.async
import scala.async.Async.await
import scala.collection.immutable.Seq
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

final class AllDocumentsStore(implicit dispatcher: Dispatcher,
                              scalaJsApiClient: ScalaJsApiClient,
                              entityAccess: JsEntityAccess,
                              getInitialDataResponse: GetInitialDataResponse)
    extends StateStore[State] {

  dispatcher.registerPartialAsync {
    case AddEmptyDocument(name, orderToken) =>
      val document = DocumentEntity(
        name = name,
        orderToken = orderToken,
        idOption = Some(EntityModification.generateRandomId()))
      entityAccess.persistModifications(
        EntityModification.Add(document),
        EntityModification.createAddWithRandomId(
          TaskEntity(
            documentId = document.id,
            contentHtml = "",
            orderToken = OrderToken.middle,
            indentation = 0,
            collapsed = false,
            delayedUntil = None,
            tags = Seq()))
      )
    case UpdateDocuments(documents) =>
      scalaJsApiClient.updateDocuments(documents)
    case RemoveDocument(existingDocument) =>
      entityAccess.persistModifications(EntityModification.createDelete(existingDocument))
  }

  StateOptionStore.register(() => AllDocumentsStore.this.invokeStateUpdateListeners())

  override def state: State = StateOptionStore.state match {
    case None    => State(allDocuments = getInitialDataResponse.allAccessibleDocuments.sorted)
    case Some(s) => s
  }

  private object StateOptionStore extends AsyncEntityDerivedStateStore[State] {

    override protected def calculateState(): Future[State] = async {
      val allDocuments = await(entityAccess.newQuery[DocumentEntity]().data())
      State(allDocuments = allDocuments.sorted)
    }

    override protected def modificationImpactsState(entityModification: EntityModification,
                                                    state: State): Boolean =
      entityModification.entityType == DocumentEntity.Type
  }
}

object AllDocumentsStore {
  case class State(allDocuments: Seq[DocumentEntity])
}