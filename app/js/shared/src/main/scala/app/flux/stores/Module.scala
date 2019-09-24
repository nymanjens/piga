package app.flux.stores

import app.api.ScalaJsApi.GetInitialDataResponse
import app.api.ScalaJsApiClient
import hydro.common.I18n
import app.flux.stores.document.AllDocumentsStore
import app.flux.stores.document.DocumentSelectionStore
import app.flux.stores.document.DocumentStoreFactory
import app.models.user.User
import hydro.common.time.Clock
import hydro.flux.action.Dispatcher
import hydro.flux.stores.ApplicationIsOnlineStore
import hydro.flux.stores.LocalDatabaseHasBeenLoadedStore
import hydro.flux.stores.PageLoadingStateStore
import hydro.flux.stores.UserStore
import hydro.models.access.HydroPushSocketClientFactory
import hydro.models.access.JsEntityAccess

final class Module(
    implicit i18n: I18n,
    user: User,
    entityAccess: JsEntityAccess,
    dispatcher: Dispatcher,
    clock: Clock,
    scalaJsApiClient: ScalaJsApiClient,
    hydroPushSocketClientFactory: HydroPushSocketClientFactory,
    getInitialDataResponse: GetInitialDataResponse,
) {

  implicit val userStore = new UserStore
  implicit val allDocumentsStore = new AllDocumentsStore
  implicit val documentStoreFactory = new DocumentStoreFactory
  implicit val documentSelectionStore = new DocumentSelectionStore
  implicit val globalMessagesStore = new GlobalMessagesStore
  implicit val pageLoadingStateStore = new PageLoadingStateStore
  implicit val pendingModificationsStore = new PendingModificationsStore
  implicit val applicationIsOnlineStore = new ApplicationIsOnlineStore
  implicit val localDatabaseHasBeenLoadedStore = new LocalDatabaseHasBeenLoadedStore
}
