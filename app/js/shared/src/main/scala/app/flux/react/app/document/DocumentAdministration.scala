package app.flux.react.app.document

import app.flux.action.AppActions
import app.flux.stores.document.AllDocumentsStore
import app.models.document.DocumentEntity
import hydro.common.I18n
import hydro.common.LoggingUtils.LogExceptionsCallback
import hydro.common.LoggingUtils.logExceptions
import hydro.common.OrderToken
import hydro.flux.action.Dispatcher
import hydro.flux.react.ReactVdomUtils.^^
import hydro.flux.react.uielements.HalfPanel
import hydro.flux.react.uielements.PageHeader
import hydro.flux.react.uielements.Table
import hydro.flux.react.HydroReactComponent
import hydro.flux.react.uielements.Bootstrap
import hydro.flux.react.uielements.Bootstrap.Size
import hydro.flux.react.uielements.Bootstrap.Variant
import hydro.flux.router.RouterContext
import hydro.models.access.EntityAccess
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.vdom.html_<^.^
import org.scalajs.dom

import scala.collection.immutable.Seq

private[app] final class DocumentAdministration(implicit entityAccess: EntityAccess,
                                                i18n: I18n,
                                                allDocumentsStore: AllDocumentsStore,
                                                dispatcher: Dispatcher,
                                                pageHeader: PageHeader,
) extends HydroReactComponent {

  // **************** API ****************//
  def apply(router: RouterContext): VdomElement = {
    component(Props(router))
  }

  // **************** Implementation of HydroReactComponent methods ****************//
  override protected val config = ComponentConfig(backendConstructor = new Backend(_), initialState = State())
    .withStateStoresDependency(allDocumentsStore, _.copy(allDocuments = allDocumentsStore.state.allDocuments))

  // **************** Implementation of HydroReactComponent types ****************//
  protected case class Props(router: RouterContext)
  protected case class State(allDocuments: Seq[DocumentEntity] = Seq(),
                             documentIdToNameInput: Map[Long, String] = Map()) {
    def nameInput(documentEntity: DocumentEntity): String = {
      if (documentIdToNameInput contains documentEntity.id) {
        documentIdToNameInput(documentEntity.id)
      } else {
        documentEntity.name
      }
    }

    def isFirst(document: DocumentEntity): Boolean = allDocuments.headOption == Some(document)
    def isLast(document: DocumentEntity): Boolean = allDocuments.lastOption == Some(document)

    def withNameInput(document: DocumentEntity, newNameInputValue: String): State = {
      copy(documentIdToNameInput = documentIdToNameInput + (document.id -> newNameInputValue))
    }
  }

  protected class Backend($ : BackendScope[Props, State]) extends BackendBase($) {

    override def render(props: Props, state: State): VdomElement = logExceptions {
      implicit val router = props.router

      <.span(
        pageHeader(router.currentPage),
        Bootstrap.Row(
          HalfPanel(title = <.span(i18n("app.all-documents"))) {
            <.span(
              Table(
                tableClasses = Seq("table-documents"),
                tableHeaders = Seq(
                  <.th(i18n("app.name")),
                  <.th()
                ),
                tableRowDatas = tableRowDatas(state)
              ),
              addButton(state)
            )
          }
        )
      )
    }

    private def tableRowDatas(implicit state: State): Seq[Table.TableRowData] = {
      for (document <- state.allDocuments) yield {
        Table.TableRowData(
          Seq[VdomElement](
            <.td(
              <.form(
                <.input(
                  ^.tpe := "text",
                  ^.name := s"document-name-${document.id}",
                  ^.value := state.nameInput(document),
                  ^^.ifThen(state.nameInput(document) != document.name) {
                    ^.className := "value-has-changed"
                  },
                  ^.autoComplete := "off",
                  ^.onChange ==> { (e: ReactEventFromInput) =>
                    logExceptions {
                      val newString = e.target.value
                      $.modState(_.withNameInput(document, newString))
                    }
                  }
                ),
                " ",
                updateNameButton(document)
              )
            ),
            <.td(
              upDownButtons(document),
              " ",
              deleteButton(document)
            )
          ))
      }
    }

    private def addButton(implicit state: State): VdomNode = {
      Bootstrap.Button(Variant.info, tag = <.a)(
        Bootstrap.FontAwesomeIcon("plus"),
        " ",
        i18n("app.create-new-document"),
        ^.onClick --> doAdd()
      )
    }

    private def updateNameButton(document: DocumentEntity)(implicit state: State): VdomNode = {
      Bootstrap.Button(Variant.info, Size.xs, tpe = "submit")(
        ^.disabled := state.nameInput(document) == document.name || state.nameInput(document).isEmpty,
        Bootstrap.FontAwesomeIcon("pencil"),
        ^.onClick ==> { (e: ReactEventFromInput) =>
          e.preventDefault()
          doUpdateName(document, state.nameInput(document))
        }
      )
    }
    private def upDownButtons(document: DocumentEntity)(implicit state: State): VdomNode = {
      <.span(
        Bootstrap.Button(Variant.info, Size.xs, tag = <.a)(
          ^.disabled := state.isFirst(document),
          Bootstrap.FontAwesomeIcon("arrow-up"),
          ^.onClick --> doUpdateOrderToken(
            document,
            orderTokenBetweenIndices(
              state.allDocuments.indexOf(document) - 2,
              state.allDocuments.indexOf(document) - 1))
        ),
        " ",
        Bootstrap.Button(Variant.info, Size.xs, tag = <.a)(
          ^.disabled := state.isLast(document),
          Bootstrap.FontAwesomeIcon("arrow-down"),
          ^.onClick --> doUpdateOrderToken(
            document,
            orderTokenBetweenIndices(
              state.allDocuments.indexOf(document) + 1,
              state.allDocuments.indexOf(document) + 2))
        )
      )
    }

    private def deleteButton(document: DocumentEntity)(implicit state: State): VdomNode = {
      Bootstrap.Button(Variant.info, Size.xs, tag = <.a)(
        Bootstrap.FontAwesomeIcon("times"),
        ^.onClick --> doDelete(document)
      )
    }

    private def doAdd()(implicit state: State): Callback = LogExceptionsCallback {
      dispatcher.dispatch(
        AppActions.AddEmptyDocument(
          name = i18n("app.untitled-document"),
          orderToken = OrderToken.middleBetween(state.allDocuments.lastOption.map(_.orderToken), None)))
    }

    private def doUpdateName(document: DocumentEntity, newName: String): Callback = LogExceptionsCallback {
      dispatcher.dispatch(AppActions.UpdateDocuments(Seq(document.copy(name = newName))))
    }

    private def doUpdateOrderToken(document: DocumentEntity, newOrderToken: OrderToken): Callback =
      LogExceptionsCallback {
        dispatcher.dispatch(AppActions.UpdateDocuments(Seq(document.copy(orderToken = newOrderToken))))
      }

    private def doDelete(document: DocumentEntity): Callback = LogExceptionsCallback {
      if (dom.window.confirm(s"Are you sure you want to delete '${document.name}'")) {
        dispatcher.dispatch(AppActions.RemoveDocument(document))
      }
    }

    private def orderTokenBetweenIndices(index1: Int, index2: Int)(implicit state: State): OrderToken = {
      def getOption(index: Int): Option[OrderToken] = {
        if (index < 0 || index >= state.allDocuments.size) {
          None
        } else {
          Some(state.allDocuments(index).orderToken)
        }
      }
      OrderToken.middleBetween(getOption(index1), getOption(index2))
    }
  }
}
