package flux.react.app.document

import common.I18n
import common.time.Clock
import hydro.flux.action.Dispatcher
import hydro.flux.stores.UserStore
import flux.stores.document.AllDocumentsStore
import flux.stores.document.DocumentStoreFactory
import flux.stores.document.DocumentSelectionStore
import hydro.flux.react.uielements.PageHeader
import models.access.JsEntityAccess
import models.user.User

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
