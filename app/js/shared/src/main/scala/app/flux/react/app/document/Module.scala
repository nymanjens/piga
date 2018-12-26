package app.flux.react.app.document

import common.I18n
import hydro.common.time.Clock
import hydro.flux.action.Dispatcher
import hydro.flux.stores.UserStore
import app.flux.stores.document.AllDocumentsStore
import app.flux.stores.document.DocumentStoreFactory
import app.flux.stores.document.DocumentSelectionStore
import hydro.flux.react.uielements.PageHeader
import app.models.access.JsEntityAccess
import app.models.user.User

final class Module(implicit i18n: I18n,
                   dispatcher: Dispatcher,
                   clock: Clock,
                   entityAccess: JsEntityAccess,
                   documentStoreFactory: DocumentStoreFactory,
                   documentSelectionStore: DocumentSelectionStore,
                   allDocumentsStore: AllDocumentsStore,
                   pageHeader: PageHeader,
) {

  private implicit lazy val taskEditor = new TaskEditor

  implicit lazy val desktopTaskList = new DesktopTaskList
  implicit lazy val documentAdministration = new DocumentAdministration
}
