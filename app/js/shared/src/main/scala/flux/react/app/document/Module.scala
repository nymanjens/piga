package flux.react.app.document

import common.I18n
import common.time.Clock
import flux.action.Dispatcher
import flux.stores.UserStore
import flux.stores.document.{AllDocumentsStore, DocumentStoreFactory, DocumentSelectionStore}
import models.access.JsEntityAccess
import models.user.User

final class Module(implicit i18n: I18n,
                   dispatcher: Dispatcher,
                   clock: Clock,
                   entityAccess: JsEntityAccess,
                   documentStoreFactory: DocumentStoreFactory,
                   documentSelectionStore: DocumentSelectionStore,
                   allDocumentsStore: AllDocumentsStore) {

  private implicit lazy val taskEditor = new TaskEditor

  implicit lazy val desktopTaskList = new DesktopTaskList
  implicit lazy val documentAdministration = new DocumentAdministration
}
