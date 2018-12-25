package flux.react.app

import common.I18n
import common.time.Clock
import flux.stores._
import flux.stores.document.AllDocumentsStore
import flux.stores.document.DocumentSelectionStore
import flux.stores.document.DocumentStoreFactory
import hydro.flux.action.Dispatcher
import hydro.flux.react.uielements.PageHeader
import hydro.flux.stores.ApplicationIsOnlineStore
import hydro.flux.stores.PageLoadingStateStore
import hydro.flux.stores.UserStore
import models.access.JsEntityAccess
import models.user.User

final class Module(implicit i18n: I18n,
                   user: User,
                   entityAccess: JsEntityAccess,
                   globalMessagesStore: GlobalMessagesStore,
                   pageLoadingStateStore: PageLoadingStateStore,
                   pendingModificationsStore: PendingModificationsStore,
                   applicationIsOnlineStore: ApplicationIsOnlineStore,
                   userStore: UserStore,
                   allDocumentsStore: AllDocumentsStore,
                   documentStoreFactory: DocumentStoreFactory,
                   documentSelectionStore: DocumentSelectionStore,
                   dispatcher: Dispatcher,
                   clock: Clock,
                   pageHeader: PageHeader,
) {

  // Configuration of submodules
  private val hydroUielementsModule = new hydro.flux.react.uielements.Module
  private val userManagementModule = new hydro.flux.react.uielements.usermanagement.Module
  private val desktopModule = new flux.react.app.document.Module

  implicit private lazy val globalMessages = hydroUielementsModule.globalMessages
  implicit private lazy val pageLoadingSpinner = hydroUielementsModule.pageLoadingSpinner
  implicit private lazy val applicationDisconnectedIcon =
    hydroUielementsModule.applicationDisconnectedIcon
  implicit private lazy val pendingModificationsCounter =
    hydroUielementsModule.pendingModificationsCounter

  implicit private lazy val menu: Menu = new Menu

  implicit lazy val layout: Layout = new Layout

  implicit lazy val userProfile = userManagementModule.userProfile
  implicit lazy val userAdministration = userManagementModule.userAdministration

  implicit lazy val desktopTaskList = desktopModule.desktopTaskList
  implicit lazy val documentAdministration = desktopModule.documentAdministration
}
