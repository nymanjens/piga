package app.flux.react.app.document

import common.I18n
import hydro.common.LoggingUtils.LogExceptionsCallback
import hydro.common.LoggingUtils.logExceptions
import app.flux.router.RouterContext
import app.flux.stores.document.DocumentStore
import app.flux.stores.document.DocumentStoreFactory
import hydro.flux.react.uielements.PageHeader
import hydro.flux.react.uielements.WaitForFuture
import hydro.flux.stores.StateStore
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import app.models.access.EntityAccess
import app.models.document.Document

private[app] final class DesktopTaskList(implicit entityAccess: EntityAccess,
                                         documentStoreFactory: DocumentStoreFactory,
                                         i18n: I18n,
                                         taskEditor: TaskEditor,
                                         pageHeader: PageHeader,
) {

  private val waitForFuture = new WaitForFuture[DocumentStore]
  private val component = ScalaComponent
    .builder[Props](getClass.getSimpleName)
    .initialStateFromProps(props => State(document = props.documentStore.state.document))
    .renderBackend[Backend]
    .componentWillMount(scope => scope.backend.willMount(scope.props, scope.state))
    .componentWillUnmount(scope => scope.backend.willUnmount(scope.props))
    .build

  // **************** API ****************//
  def apply(documentId: Long, router: RouterContext): VdomElement = {
    waitForFuture(documentStoreFactory.create(documentId)) { documentStore =>
      component(Props(documentStore, router))
    }
  }

  // **************** Private inner types ****************//
  private case class Props(documentStore: DocumentStore, router: RouterContext)
  private case class State(document: Document)

  private class Backend($ : BackendScope[Props, State]) extends StateStore.Listener {

    def willMount(props: Props, state: State): Callback = LogExceptionsCallback {
      props.documentStore.register(this)
      $.modState(state => logExceptions(state.copy(document = props.documentStore.state.document))).runNow()
    }

    def willUnmount(props: Props): Callback = LogExceptionsCallback {
      props.documentStore.deregister(this)
    }

    override def onStateUpdate() = {
      val props = $.props.runNow()
      $.modState(state => logExceptions(state.copy(document = props.documentStore.state.document))).runNow()
    }

    def render(props: Props, state: State): VdomElement = logExceptions {
      implicit val router = props.router

      <.span(
        pageHeader(router.currentPage, title = state.document.name),
        taskEditor(props.documentStore)
      )
    }
  }
}
