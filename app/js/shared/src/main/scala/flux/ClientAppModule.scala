package flux

import api.ScalaJsApi.GetInitialDataResponse
import api.ScalaJsApiClient
import flux.router.Page
import hydro.flux.action.Module
import japgolly.scalajs.react.extra.router.Router
import models.user.User

final class ClientAppModule(implicit getInitialDataResponse: GetInitialDataResponse,
                            scalaJsApiClient: ScalaJsApiClient) {

  // Unpack arguments
  implicit private val user: User = getInitialDataResponse.user

  // Create and unpack common modules
  private val commonTimeModule = new common.time.Module
  implicit private val clock = commonTimeModule.clock
  private val commonModule = new common.Module
  implicit private val i18n = commonModule.i18n

  // Create and unpack Models Access module
  val modelsAccessModule = new models.access.Module
  implicit val entityAccess = modelsAccessModule.entityAccess
  implicit val entityModificationPushClientFactory = modelsAccessModule.entityModificationPushClientFactory

  // Create and unpack Flux action module
  private val fluxActionModule = new Module
  implicit private val dispatcher = fluxActionModule.dispatcher

  // Create and unpack Flux store module
  private val fluxStoresModule = new flux.stores.Module
  implicit private val globalMessagesStore = fluxStoresModule.globalMessagesStore
  implicit private val pageLoadingStateStore = fluxStoresModule.pageLoadingStateStore
  implicit private val pendingModificationsStore = fluxStoresModule.pendingModificationsStore
  implicit private val applicationIsOnlineStore = fluxStoresModule.applicationIsOnlineStore
  implicit private val userStore = fluxStoresModule.userStore
  implicit private val allDocumentsStore = fluxStoresModule.allDocumentsStore
  implicit private val documentStoreFactory = fluxStoresModule.documentStoreFactory
  implicit private val documentSelectionStore = fluxStoresModule.documentSelectionStore

  // Create and unpack Flux Hydro react modules
  private lazy val hydroUielementsModule = new hydro.flux.react.uielements.Module
  implicit private lazy val pageHeader = hydroUielementsModule.pageHeader
  implicit private lazy val globalMessages = hydroUielementsModule.globalMessages
  implicit private lazy val pageLoadingSpinner = hydroUielementsModule.pageLoadingSpinner
  implicit private lazy val applicationDisconnectedIcon = hydroUielementsModule.applicationDisconnectedIcon
  implicit private lazy val pendingModificationsCounter = hydroUielementsModule.pendingModificationsCounter
  private lazy val hydroUsermanagementModule = new hydro.flux.react.uielements.usermanagement.Module
  implicit private lazy val userProfile = hydroUsermanagementModule.userProfile
  implicit private lazy val userAdministration = hydroUsermanagementModule.userAdministration

  // Create other Flux modules
  implicit private val reactAppModule = new flux.react.app.Module
  implicit private val routerModule = new flux.router.Module

  val router: Router[Page] = routerModule.router
}
