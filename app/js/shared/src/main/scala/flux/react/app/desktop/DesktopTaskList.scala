package flux.react.app.desktop

import common.I18n
import common.LoggingUtils.logExceptions
import flux.react.router.RouterContext
import flux.react.uielements
import flux.stores.document.{DocumentStore, DocumentStoreFactory}
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import models.access.EntityAccess

private[app] final class DesktopTaskList(implicit entityAccess: EntityAccess,
                                         documentStoreFactory: DocumentStoreFactory,
                                         i18n: I18n,
                                         taskEditor: TaskEditor) {

  private val waitForFuture = new uielements.WaitForFuture[DocumentStore]
  private val component = ScalaComponent
    .builder[Props](getClass.getSimpleName)
    .renderBackend[Backend]
    .build

  // **************** API ****************//
  def apply(documentId: Long, router: RouterContext): VdomElement = {
    component(Props(documentId, router))
  }

  // **************** Private inner types ****************//
  private case class Props(documentId: Long, router: RouterContext)
  private type State = Unit

  private class Backend($ : BackendScope[Props, State]) {
    def render(props: Props, state: State): VdomElement = logExceptions {
      implicit val router = props.router

      <.span(waitForFuture(documentStoreFactory.create(props.documentId)) { documentStore =>
        <.span(
          uielements.PageHeader(router.currentPage, title = documentStore.document.name),
          taskEditor()
        )
      })
    }
  }
}
