package app.flux.react.app.document

import app.flux.stores.document.AllDocumentsStore
import app.flux.stores.document.DocumentSelectionStore
import app.flux.stores.document.DocumentStoreFactory
import hydro.common.I18n
import hydro.common.time.Clock
import hydro.flux.action.Dispatcher
import hydro.flux.react.uielements.PageHeader
import hydro.models.access.JsEntityAccess

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

  implicit lazy val taskList = new TaskList
  implicit lazy val documentAdministration = new DocumentAdministration
}
