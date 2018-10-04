package flux.stores.document

import api.ScalaJsApi.GetInitialDataResponse
import api.ScalaJsApiClient
import common.OrderToken
import flux.action.Action._
import flux.action.Dispatcher
import flux.stores.document.AllDocumentsStore.State
import flux.stores.{AsyncEntityDerivedStateStore, StateStore}
import models.access.JsEntityAccess
import models.document
import models.document.{DocumentEntity, TaskEntity}
import models.modification.{EntityModification, EntityType}

import scala.async.Async.{async, await}
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
      entityModification.entityType == EntityType.DocumentEntityType
  }
}

object AllDocumentsStore {
  case class State(allDocuments: Seq[DocumentEntity])
}
