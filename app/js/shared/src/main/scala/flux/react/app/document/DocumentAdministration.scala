package flux.react.app.document

import flux.react.ReactVdomUtils.^^
import common.I18n
import common.OrderToken
import common.LoggingUtils.LogExceptionsCallback
import common.LoggingUtils.logExceptions
import flux.action.Action
import hydro.flux.action.Dispatcher
import flux.react.ReactVdomUtils.<<
import flux.react.ReactVdomUtils.^^
import flux.react.router.RouterContext
import flux.react.uielements
import hydro.flux.stores.StateStore
import flux.stores.document.AllDocumentsStore
import flux.stores.document.DocumentStore
import flux.stores.document.DocumentStoreFactory
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^.^
import japgolly.scalajs.react.vdom.html_<^._
import models.access.EntityAccess
import models.document.Document
import models.document.DocumentEntity
import models.user.User
import org.scalajs.dom

import scala.collection.immutable.Seq
import scala.scalajs.js

private[app] final class DocumentAdministration(implicit entityAccess: EntityAccess,
                                                i18n: I18n,
                                                allDocumentsStore: AllDocumentsStore,
                                                dispatcher: Dispatcher) {

  private val component = ScalaComponent
    .builder[Props](getClass.getSimpleName)
    .initialState(State(allDocuments = allDocumentsStore.state.allDocuments, documentIdToNameInput = Map()))
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
  private case class State(allDocuments: Seq[DocumentEntity], documentIdToNameInput: Map[Long, String]) {
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
        <.div(
          ^.className := "row",
          uielements.HalfPanel(title = <.span(i18n("app.all-documents"))) {
            <.span(
              uielements.Table(
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

    private def tableRowDatas(implicit state: State): Seq[uielements.Table.TableRowData] = {
      for (document <- state.allDocuments) yield {
        uielements.Table.TableRowData(
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
      <.a(
        ^.className := "btn btn-info",
        <.i(^.className := "fa fa-plus"),
        " ",
        i18n("app.create-new-document"),
        ^.onClick --> doAdd()
      )
    }

    private def updateNameButton(document: DocumentEntity)(implicit state: State): VdomNode = {
      <.button(
        ^.tpe := "submit",
        ^.className := "btn btn-info btn-xs",
        ^.disabled := state.nameInput(document) == document.name || state.nameInput(document).isEmpty,
        <.i(^.className := "fa fa-pencil"),
        ^.onClick ==> { (e: ReactEventFromInput) =>
          e.preventDefault()
          doUpdateName(document, state.nameInput(document))
        }
      )
    }
    private def upDownButtons(document: DocumentEntity)(implicit state: State): VdomNode = {
      <.span(
        <.a(
          ^.className := "btn btn-info btn-xs",
          ^.disabled := state.isFirst(document),
          <.i(^.className := "fa fa-arrow-up"),
          ^.onClick --> doUpdateOrderToken(
            document,
            orderTokenBetweenIndices(
              state.allDocuments.indexOf(document) - 2,
              state.allDocuments.indexOf(document) - 1))
        ),
        " ",
        <.a(
          ^.className := "btn btn-info btn-xs",
          ^.disabled := state.isLast(document),
          <.i(^.className := "fa fa-arrow-down"),
          ^.onClick --> doUpdateOrderToken(
            document,
            orderTokenBetweenIndices(
              state.allDocuments.indexOf(document) + 1,
              state.allDocuments.indexOf(document) + 2))
        )
      )
    }

    private def deleteButton(document: DocumentEntity)(implicit state: State): VdomNode = {
      <.a(
        ^.className := "btn btn-info btn-xs",
        <.i(^.className := "fa fa-times"),
        ^.onClick --> doDelete(document)
      )
    }

    private def doAdd()(implicit state: State): Callback = LogExceptionsCallback {
      dispatcher.dispatch(
        Action.AddEmptyDocument(
          name = i18n("app.untitled-document"),
          orderToken = OrderToken.middleBetween(state.allDocuments.lastOption.map(_.orderToken), None)))
    }

    private def doUpdateName(document: DocumentEntity, newName: String): Callback = LogExceptionsCallback {
      dispatcher.dispatch(Action.UpdateDocuments(Seq(document.copy(name = newName))))
    }

    private def doUpdateOrderToken(document: DocumentEntity, newOrderToken: OrderToken): Callback =
      LogExceptionsCallback {
        dispatcher.dispatch(Action.UpdateDocuments(Seq(document.copy(orderToken = newOrderToken))))
      }

    private def doDelete(document: DocumentEntity): Callback = LogExceptionsCallback {
      if (dom.window.confirm(s"Are you sure you want to delete '${document.name}'")) {
        dispatcher.dispatch(Action.RemoveDocument(document))
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
