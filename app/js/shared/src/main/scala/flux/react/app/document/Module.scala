package flux.react.app.document

import common.I18n
import common.time.Clock
import flux.action.Dispatcher
import flux.stores.UserStore
import flux.stores.document.DocumentStoreFactory
import models.access.JsEntityAccess
import models.user.User

final class Module(implicit i18n: I18n,
                   dispatcher: Dispatcher,
                   clock: Clock,
                   entityAccess: JsEntityAccess,
                   documentStoreFactory: DocumentStoreFactory) {

  private implicit lazy val taskEditor = new TaskEditor

  implicit lazy val desktopTaskList = new DesktopTaskList
}
