package flux.stores

import api.ScalaJsApi.GetInitialDataResponse
import api.ScalaJsApiClient
import common.I18n
import common.time.Clock
import flux.action.Dispatcher
import flux.stores.document.{AllDocumentsStore, DocumentSelectionStore, DocumentStoreFactory}
import models.access.{EntityModificationPushClientFactory, JsEntityAccess}
import models.user.User

final class Module(implicit i18n: I18n,
                   user: User,
                   entityAccess: JsEntityAccess,
                   dispatcher: Dispatcher,
                   clock: Clock,
                   scalaJsApiClient: ScalaJsApiClient,
                   entityModificationPushClientFactory: EntityModificationPushClientFactory,
                   getInitialDataResponse: GetInitialDataResponse) {

  implicit val userStore = new UserStore
  implicit val allDocumentsStore = new AllDocumentsStore
  implicit val documentStoreFactory = new DocumentStoreFactory
  implicit val documentSelectionStore = new DocumentSelectionStore
  implicit val globalMessagesStore = new GlobalMessagesStore
  implicit val pageLoadingStateStore = new PageLoadingStateStore
  implicit val pendingModificationsStore = new PendingModificationsStore
  implicit val applicationIsOnlineStore = new ApplicationIsOnlineStore
}
