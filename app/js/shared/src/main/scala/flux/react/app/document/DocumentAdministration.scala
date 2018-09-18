package flux.react.app.document

import common.I18n
import common.LoggingUtils.{LogExceptionsCallback, logExceptions}
import flux.react.ReactVdomUtils.<<
import flux.react.router.RouterContext
import flux.react.uielements
import flux.stores.StateStore
import flux.stores.document.{AllDocumentsStore, DocumentStore, DocumentStoreFactory}
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import models.access.EntityAccess
import models.document.{Document, DocumentEntity}
import models.user.User

import scala.collection.immutable.Seq
import scala.scalajs.js

private[app] final class DocumentAdministration(implicit entityAccess: EntityAccess,
                                                i18n: I18n,
                                                allDocumentsStore: AllDocumentsStore) {

  private val component = ScalaComponent
    .builder[Props](getClass.getSimpleName)
    .initialState(State(allDocuments = allDocumentsStore.state.allDocuments))
    .renderBackend[Backend]
    .componentWillMount(scope => scope.backend.willMount(scope.state))
    .componentWillUnmount(scope => scope.backend.willUnmount())
    .build

  // **************** API ****************//
  def apply(router: RouterContext): VdomElement = {
    component(Props(router))
  }

  // **************** Private inner types ****************//
  private case class Props(router: RouterContext)
  private case class State(allDocuments: Seq[DocumentEntity])

  private class Backend($ : BackendScope[Props, State]) extends StateStore.Listener {

    def willMount(state: State): Callback = LogExceptionsCallback {
      allDocumentsStore.register(this)
      $.modState(state => logExceptions(state.copy(allDocuments = allDocumentsStore.state.allDocuments)))
        .runNow()
    }

    def willUnmount(): Callback = LogExceptionsCallback {
      allDocumentsStore.deregister(this)
    }

    override def onStateUpdate() = {
      $.modState(state => logExceptions(state.copy(allDocuments = allDocumentsStore.state.allDocuments)))
        .runNow()
    }

    def render(props: Props, state: State): VdomElement = logExceptions {
      implicit val router = props.router

      <.span(
        uielements.PageHeader(router.currentPage),
        uielements.HalfPanel(title = <.span(i18n("app.all-documents"))) {
          uielements.Table(
            tableHeaders = Seq(
              <.th(i18n("app.name")),
              <.th()
            ),
            tableRowDatas = tableRowDatas(state)
          )
        }
      )
    }

    private def tableRowDatas(state: State): Seq[uielements.Table.TableRowData] = {
      for (document <- state.allDocuments) yield {
        uielements.Table.TableRowData(
          Seq[VdomElement](
            <.td(document.name),
            <.td(<.i(^.className := "fa fa-check"))
          ))
      }
    }
  }
}
