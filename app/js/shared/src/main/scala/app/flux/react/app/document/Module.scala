package app.flux.react.app.document

import app.flux.stores.document.AllDocumentsStore
import app.flux.stores.document.DocumentSelectionStore
import app.flux.stores.document.DocumentStoreFactory
import app.models.user.User
import hydro.common.I18n
import hydro.common.time.Clock
import hydro.flux.action.Dispatcher
import hydro.flux.react.uielements.PageHeader
import hydro.models.access.JsEntityAccess
import app.flux.stores.GlobalMessagesStore

final class Module(implicit
    i18n: I18n,
    user: User,
    dispatcher: Dispatcher,
    clock: Clock,
    globalMessagesStore: GlobalMessagesStore,
    entityAccess: JsEntityAccess,
    documentStoreFactory: DocumentStoreFactory,
    documentSelectionStore: DocumentSelectionStore,
    allDocumentsStore: AllDocumentsStore,
    pageHeader: PageHeader,
) {

  private implicit lazy val editHistory = new EditHistory

  private implicit lazy val desktopTaskEditor = new DesktopTaskEditor
  private implicit lazy val mobileTaskEditor = new MobileTaskEditor

  implicit lazy val taskList = new TaskList
  implicit lazy val documentAdministration = new DocumentAdministration
}
