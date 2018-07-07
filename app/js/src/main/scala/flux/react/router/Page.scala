package flux.react.router

import common.I18n
import japgolly.scalajs.react.extra.router.Path
import models.accounting.BalanceCheck
import models.accounting.config.{Account, MoneyReservoir, Template}

import scala.scalajs.js

sealed trait Page {
  def title(implicit i18n: I18n): String
  def iconClass: String
}
object Page {

  sealed abstract class PageBase(titleKey: String, override val iconClass: String) extends Page {
    override def title(implicit i18n: I18n) = i18n(titleKey)
  }

  sealed abstract class HasReturnTo(private val encodedReturnTo: Option[String]) {
    def returnToPath: Path =
      Path(RouterFactory.pathPrefix + js.URIUtils.decodeURIComponent(encodedReturnTo getOrElse ""))
  }
  private object HasReturnTo {
    def getCurrentEncodedPath(implicit routerContext: RouterContext): Option[String] = Some {
      val path = routerContext
        .toPath(routerContext.currentPage)
        .removePrefix(RouterFactory.pathPrefix)
        .get
        .value
      js.URIUtils.encodeURIComponent(
        // Decode path first because routerContext.toPath() seems to produce unnecessarily and
        // inconsistently escaped strings
        js.URIUtils.decodeURIComponent(path))
    }
  }

  case object Root extends Page {
    override def title(implicit i18n: I18n) = "Root"
    override def iconClass = ""
  }

  // **************** User management views **************** //
  case object UserProfile extends PageBase("app.user-profile", iconClass = "fa fa-user fa-fw")
  case object UserAdministration extends PageBase("app.user-administration", iconClass = "fa fa-cogs fa-fw")

  // **************** Task lists **************** //
  case object DesktopTaskList extends PageBase("Piga Task List", iconClass = "icon-list")
}
