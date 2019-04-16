package app.flux.router

import hydro.common.I18n
import app.models.document.DocumentEntity
import hydro.flux.router.Page
import hydro.flux.router.Page.PageBase
import hydro.models.access.EntityAccess

import scala.async.Async.async
import scala.async.Async.await
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

object AppPages {

  // **************** Document views **************** //
  case object DocumentAdministration
      extends PageBase("app.document-administration", iconClass = "fa fa-pencil fa-fw")
  case class TaskList(documentId: Long) extends Page {
    override def title(implicit i18n: I18n, entityAccess: EntityAccess) = async {
      await(entityAccess.newQuery[DocumentEntity]().findById(documentId)).name
    }
    override def iconClass = "icon-list"
  }
  case class MobileTaskList(documentId: Long) extends Page {
    override def title(implicit i18n: I18n, entityAccess: EntityAccess) = async {
      await(entityAccess.newQuery[DocumentEntity]().findById(documentId)).name
    }
    override def iconClass = "fa fa-mobile"
  }
}
