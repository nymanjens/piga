package app.flux.router

import scala.async.Async.{async, await}
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent.Future
import common.I18n
import japgolly.scalajs.react.extra.router.Path
import app.models.access.EntityAccess
import app.models.document.DocumentEntity

import scala.scalajs.js

sealed trait Page {
  def title(implicit i18n: I18n, entityAccess: EntityAccess): Future[String]
  def iconClass: String
}
object Page {

  sealed abstract class PageBase(titleKey: String, override val iconClass: String) extends Page {
    override def title(implicit i18n: I18n, entityAccess: EntityAccess) = Future.successful(titleSync)
    def titleSync(implicit i18n: I18n) = i18n(titleKey)
  }

  case object Root extends Page {
    override def title(implicit i18n: I18n, entityAccess: EntityAccess) = Future.successful("Root")
    override def iconClass = ""
  }

  // **************** User management views **************** //
  case object UserProfile extends PageBase("app.user-profile", iconClass = "fa fa-user fa-fw")
  case object UserAdministration extends PageBase("app.user-administration", iconClass = "fa fa-cogs fa-fw")

  // **************** Document views **************** //
  case object DocumentAdministration
      extends PageBase("app.document-administration", iconClass = "fa fa-pencil fa-fw")
  case class DesktopTaskList(documentId: Long) extends Page {
    override def title(implicit i18n: I18n, entityAccess: EntityAccess) = async {
      await(entityAccess.newQuery[DocumentEntity]().findById(documentId)).name
    }
    override def iconClass = "icon-list"
  }
}
