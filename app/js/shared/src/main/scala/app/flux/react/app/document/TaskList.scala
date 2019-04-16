package app.flux.react.app.document

import app.flux.react.MobileUtils
import app.flux.stores.document.DocumentStore
import app.flux.stores.document.DocumentStoreFactory
import app.models.document.Document
import hydro.common.I18n
import hydro.common.LoggingUtils.logExceptions
import hydro.flux.react.uielements.PageHeader
import hydro.flux.react.uielements.WaitForFuture
import hydro.flux.react.HydroReactComponent
import hydro.flux.router.RouterContext
import hydro.models.access.EntityAccess
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

private[app] final class TaskList(implicit entityAccess: EntityAccess,
                                  documentStoreFactory: DocumentStoreFactory,
                                  i18n: I18n,
                                  desktopTaskEditor: DesktopTaskEditor,
                                  mobileTaskEditor: MobileTaskEditor,
                                  pageHeader: PageHeader,
) extends HydroReactComponent {

  private val waitForFuture = new WaitForFuture[DocumentStore]

  // **************** API ****************//
  def apply(documentId: Long, mobileOrTabletVersion: Boolean, router: RouterContext): VdomElement = {
    waitForFuture(documentStoreFactory.create(documentId)) { documentStore =>
      component(Props(documentStore, mobileOrTabletVersion, router))
    }
  }

  // **************** Implementation of HydroReactComponent methods ****************//
  override protected val config = ComponentConfig(backendConstructor = new Backend(_), initialState = State())
    .withStateStoresDependencyFromProps { props =>
      val store = props.documentStore
      StateStoresDependency(store, _.copy(document = store.state.document))
    }

  // **************** Implementation of HydroReactComponent types ****************//
  protected case class Props(documentStore: DocumentStore,
                             mobileOrTabletVersion: Boolean,
                             router: RouterContext)
  protected case class State(document: Document = Document.nullInstance)

  protected class Backend($ : BackendScope[Props, State]) extends BackendBase($) {

    override def render(props: Props, state: State): VdomElement = logExceptions {
      implicit val router = props.router

      <.span(
        pageHeader(router.currentPage, title = state.document.name),
        if (props.mobileOrTabletVersion) mobileTaskEditor(props.documentStore)
        else desktopTaskEditor(props.documentStore)
      )
    }
  }
}
