package app.flux.stores.document

import hydro.models.access.DbQueryImplicits._
import app.api.ScalaJsApi.GetInitialDataResponse
import app.api.ScalaJsApiClient
import app.common.document.UserDocument
import app.flux.action.AppActions.AddEmptyDocument
import app.flux.action.AppActions.RemoveDocument
import app.flux.action.AppActions.UpdateDocuments
import app.flux.stores.document.AllDocumentsStore.State
import app.models.access.ModelFields
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

final class AllDocumentsStore(implicit
    dispatcher: Dispatcher,
    user: User,
    clock: Clock,
    scalaJsApiClient: ScalaJsApiClient,
    entityAccess: JsEntityAccess,
    getInitialDataResponse: GetInitialDataResponse,
) extends StateStore[State] {

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
            checked = false,
            delayedUntil = None,
            tags = Seq(),
            lastContentModifierUserId = user.id,
          )
        ),
      )
    case UpdateDocuments(userDocuments) =>
      async {

        val updateFutures =
          for (userDocument <- userDocuments)
            yield async {
              val documentEntityUpdate = {
                val documentEntity =
                  await(entityAccess.newQuery[DocumentEntity]().findById(userDocument.documentId))
                val newDocumentEntity =
                  documentEntity.copy(name = userDocument.name)
                if (documentEntity == newDocumentEntity) {
                  None
                } else {
                  Some(EntityModification.createUpdateAllFields(newDocumentEntity))
                }
              }

              val permissionAndPlacementUpdate = {
                val permissionAndPlacement = await(fetchPermissionAndPlacement(userDocument))
                val newPermissionAndPlacement =
                  permissionAndPlacement.copy(orderToken = userDocument.orderToken)
                if (permissionAndPlacement == newPermissionAndPlacement) {
                  None
                } else {
                  Some(EntityModification.createUpdateAllFields(newPermissionAndPlacement))
                }
              }

              Seq() ++ documentEntityUpdate ++ permissionAndPlacementUpdate
            }

        val updates = await(Future.sequence(updateFutures)).flatten

        entityAccess.persistModifications(updates)
      }

    case RemoveDocument(existingDocument) =>
      async {
        // Don't delete DocumentEntity / TaskEntities, so that:
        // - Shared documents don't get deleted
        // - There is a way to restore accidentally deleted documents
        //
        // From the above, it follows that we should only delete this user's permission for this
        // document, not the other users' permissions
        val permissionAndPlacement = await(fetchPermissionAndPlacement(existingDocument))

        entityAccess.persistModifications(EntityModification.createRemove(permissionAndPlacement))
      }
  }

  StateOptionStore.register(() => AllDocumentsStore.this.invokeStateUpdateListeners())

  override def state: State = StateOptionStore.state match {
    case None    => State(allDocuments = getInitialDataResponse.allAccessibleDocuments)
    case Some(s) => s
  }

  private def fetchPermissionAndPlacement(
      userDocument: UserDocument
  ): Future[DocumentPermissionAndPlacement] = async {
    await(
      entityAccess
        .newQuery[DocumentPermissionAndPlacement]()
        .filter(ModelFields.DocumentPermissionAndPlacement.documentId === userDocument.documentId)
        .findOne(ModelFields.DocumentPermissionAndPlacement.userId === user.id)
    ).get
  }

  private object StateOptionStore extends AsyncEntityDerivedStateStore[State] {

    override protected def calculateState(): Future[State] = async {
      val allDocuments = await(UserDocument.fetchAllForUser())
      State(allDocuments = allDocuments)
    }

    override protected def modificationImpactsState(
        entityModification: EntityModification,
        state: State,
    ): Boolean = {
      entityModification.entityType == DocumentEntity.Type || entityModification.entityType == DocumentPermissionAndPlacement.Type
    }

  }
}

object AllDocumentsStore {
  case class State(allDocuments: Seq[UserDocument])
}
