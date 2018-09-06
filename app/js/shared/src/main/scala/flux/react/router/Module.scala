package flux.react.router

import common.I18n
import flux.action.Dispatcher
import flux.stores.document.AllDocumentsStore
import japgolly.scalajs.react.extra.router._

final class Module(implicit reactAppModule: flux.react.app.Module,
                   dispatcher: Dispatcher,
                   i18n: I18n,
                   allDocumentsStore: AllDocumentsStore) {

  implicit lazy val router: Router[Page] = (new RouterFactory).createRouter()
}
