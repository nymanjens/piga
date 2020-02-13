package app.flux.react.app

import hydro.common.I18n
import app.flux.stores._
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
import hydro.models.access.JsEntityAccess

final class Module(implicit i18n: I18n,
                   user: User,
                   entityAccess: JsEntityAccess,
                   globalMessagesStore: GlobalMessagesStore,
                   pageLoadingStateStore: PageLoadingStateStore,
                   pendingModificationsStore: PendingModificationsStore,
                   applicationIsOnlineStore: ApplicationIsOnlineStore,
                   localDatabaseHasBeenLoadedStore: LocalDatabaseHasBeenLoadedStore,
                   userStore: UserStore,
                   allDocumentsStore: AllDocumentsStore,
                   documentStoreFactory: DocumentStoreFactory,
                   documentSelectionStore: DocumentSelectionStore,
                   dispatcher: Dispatcher,
                   clock: Clock,
) {

  // Configuration of submodules
  private val hydroUielementsModule = new hydro.flux.react.uielements.Module
  implicit private lazy val pageHeader = hydroUielementsModule.pageHeader
  implicit private lazy val sbadminMenu = hydroUielementsModule.sbadminMenu
  implicit private lazy val sbadminLayout = hydroUielementsModule.sbadminLayout

  private val userManagementModule = new hydro.flux.react.uielements.usermanagement.Module
  private val databaseExplorerModule = new hydro.flux.react.uielements.dbexplorer.Module
  private val documentModule = new app.flux.react.app.document.Module

  implicit private lazy val menu: Menu = new Menu

  implicit lazy val layout: Layout = new Layout

  implicit lazy val userProfile = userManagementModule.userProfile
  implicit lazy val userAdministration = userManagementModule.userAdministration
  implicit lazy val databaseExplorer = databaseExplorerModule.databaseExplorer

  implicit lazy val taskList = documentModule.taskList
  implicit lazy val documentAdministration = documentModule.documentAdministration
}
