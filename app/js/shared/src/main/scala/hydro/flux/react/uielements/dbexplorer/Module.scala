package hydro.flux.react.uielements.dbexplorer

import app.models.user.User
import hydro.common.I18n
import hydro.common.time.Clock
import hydro.flux.action.Dispatcher
import hydro.flux.react.uielements.PageHeader
import hydro.flux.stores.UserStore

final class Module(
    implicit i18n: I18n,
    user: User,
    dispatcher: Dispatcher,
    clock: Clock,
    pageHeader: PageHeader,
) {

  lazy val databaseExplorer: DatabaseExplorer = new DatabaseExplorer
}
