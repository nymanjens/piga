package app.flux.stores.document

import app.api.ScalaJsApi.GetInitialDataResponse
import app.api.ScalaJsApiClient
import app.common.document.UserDocument
import app.flux.action.AppActions.AddEmptyDocument
import app.flux.stores.document.AllDocumentsStore.State
import app.models.document.DocumentEntity
import app.models.document.DocumentPermissionAndPlacement
import app.models.document.TaskEntity
import app.models.user.User
import hydro.common.time.Clock
import hydro.common.OrderToken
import hydro.flux.action.Dispatcher
import hydro.flux.stores.AsyncEntityDerivedStateStore
import hydro.flux.stores.StateStore
import hydro.models.access.JsEntityAccess
import hydro.models.modification.EntityModification

import scala.async.Async.async
import scala.async.Async.await
import scala.collection.immutable.Seq
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

final class AllDocumentsStore(
    implicit dispatcher: Dispatcher,
    user: User,
    clock: Clock,
    scalaJsApiClient: ScalaJsApiClient,
    entityAccess: JsEntityAccess,
    getInitialDataResponse: GetInitialDataResponse,
) extends StateStore[State] {

  // TODO(feat-sharing): Re-enable this
  dispatcher.registerPartialAsync {
    case AddEmptyDocument(name, orderToken) =>
      val document = DocumentEntity(name = name, idOption = Some(EntityModification.generateRandomId()))
      val permissionAndPlacement = DocumentPermissionAndPlacement(
        documentId = document.id,
        userId = user.id,
        orderToken = orderToken,
      )
      entityAccess.persistModifications(
        EntityModification.Add(document),
        EntityModification.createAddWithRandomId(permissionAndPlacement),
        EntityModification.createAddWithRandomId(
          TaskEntity(
            documentId = document.id,
            contentHtml = "",
            orderToken = OrderToken.middle,
            indentation = 0,
            collapsed = false,
            delayedUntil = None,
            tags = Seq(),
            lastContentModifierUserId = user.id
          ))
      )
//    case UpdateDocuments(documents) =>
//      entityAccess.persistModifications(documents.map(d => EntityModification.createUpdateAllFields(d)))
//    case RemoveDocument(existingDocument) =>
//      entityAccess.persistModifications(EntityModification.createRemove(existingDocument))
  }

  StateOptionStore.register(() => AllDocumentsStore.this.invokeStateUpdateListeners())

  override def state: State = StateOptionStore.state match {
    case None    => State(allDocuments = getInitialDataResponse.allAccessibleDocuments)
    case Some(s) => s
  }

  private object StateOptionStore extends AsyncEntityDerivedStateStore[State] {

    override protected def calculateState(): Future[State] = async {
      val allDocuments = await(UserDocument.fetchAllForUser())
      State(allDocuments = allDocuments)
    }

    override protected def modificationImpactsState(
        entityModification: EntityModification,
        state: State,
    ): Boolean =
      entityModification.entityType == DocumentEntity.Type || entityModification.entityType == DocumentPermissionAndPlacement.Type

  }
}

object AllDocumentsStore {
  case class State(allDocuments: Seq[UserDocument])
}
