package flux.react.app

import common.I18n
import common.time.Clock
import flux.stores._
import flux.stores.document.{AllDocumentsStore, DocumentSelectionStore, DocumentStoreFactory}
import hydro.flux.action.Dispatcher
import hydro.flux.stores.{ApplicationIsOnlineStore, PageLoadingStateStore, UserStore}
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
                   clock: Clock) {

  // Configuration of submodules
  private val hydroSbadminUielementsModule = new hydro.flux.react.uielements.sbadmin.Module
  private val userManagementModule = new hydro.flux.react.uielements.sbadmin.usermanagement.Module
  private val desktopModule = new flux.react.app.document.Module

  implicit private lazy val globalMessages = hydroSbadminUielementsModule.globalMessages
  implicit private lazy val pageLoadingSpinner = hydroSbadminUielementsModule.pageLoadingSpinner
  implicit private lazy val applicationDisconnectedIcon =
    hydroSbadminUielementsModule.applicationDisconnectedIcon
  implicit private lazy val pendingModificationsCounter =
    hydroSbadminUielementsModule.pendingModificationsCounter

  implicit private lazy val menu: Menu = new Menu

  implicit lazy val layout: Layout = new Layout

  implicit lazy val userProfile = userManagementModule.userProfile
  implicit lazy val userAdministration = userManagementModule.userAdministration

  implicit lazy val desktopTaskList = desktopModule.desktopTaskList
  implicit lazy val documentAdministration = desktopModule.documentAdministration
}
