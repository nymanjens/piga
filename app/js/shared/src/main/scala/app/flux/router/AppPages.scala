package app.flux.router

import app.models.access.EntityAccess
import app.models.document.DocumentEntity
import common.I18n
import hydro.flux.router.Page
import hydro.flux.router.Page.PageBase

import scala.async.Async.{async, await}
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue


object AppPages {

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
