package flux.stores

import api.ScalaJsApi.GetInitialDataResponse
import flux.stores.AllDocumentsStore.State
import models.access.JsEntityAccess
import models.document.DocumentEntity
import models.modification.{EntityModification, EntityType}

import scala.async.Async.{async, await}
import scala.collection.immutable.Seq
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

final class AllDocumentsStore(implicit entityAccess: JsEntityAccess,
                              getInitialDataResponse: GetInitialDataResponse)
    extends StateStore[State] {

  StateOptionStore.register(() => AllDocumentsStore.this.invokeStateUpdateListeners())

  override def state: State = StateOptionStore.state match {
    case None    => State(allDocuments = getInitialDataResponse.allAccessibleDocuments)
    case Some(s) => s
  }

  private object StateOptionStore extends AsyncEntityDerivedStateStore[State] {

    override protected def calculateState(): Future[State] = async {
      val allDocuments = await(entityAccess.newQuery[DocumentEntity]().data())
      State(allDocuments = allDocuments)
    }

    override protected def modificationImpactsState(entityModification: EntityModification,
                                                    state: State): Boolean =
      entityModification.entityType == EntityType.DocumentEntityType
  }
}

object AllDocumentsStore {
  case class State(allDocuments: Seq[DocumentEntity])
}
