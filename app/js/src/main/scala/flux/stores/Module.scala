package flux.stores

import api.ScalaJsApiClient
import common.I18n
import common.money.ExchangeRateManager
import common.time.Clock
import flux.action.Dispatcher
import flux.stores.entries._
import flux.stores.entries.factories._
import models.access.{EntityModificationPushClientFactory, JsEntityAccess}
import models.accounting.config.Config
import models.user.User

final class Module(implicit i18n: I18n,
                   accountingConfig: Config,
                   user: User,
                   entityAccess: JsEntityAccess,
                   exchangeRateManager: ExchangeRateManager,
                   dispatcher: Dispatcher,
                   clock: Clock,
                   scalaJsApiClient: ScalaJsApiClient,
                   entityModificationPushClientFactory: EntityModificationPushClientFactory) {

  implicit private val complexQueryFilter = new ComplexQueryFilter

  implicit val globalMessagesStore = new GlobalMessagesStore
  implicit val pageLoadingStateStore = new PageLoadingStateStore
  implicit val pendingModificationsStore = new PendingModificationsStore
  implicit val applicationIsOnlineStore = new ApplicationIsOnlineStore
  implicit val userStore = new UserStore
}
