package flux.react.app

import common.I18n
import common.LoggingUtils.{LogExceptionsCallback, logExceptions}
import common.time.Clock
import flux.react.ReactVdomUtils.^^
import flux.react.router.{Page, RouterContext}
import flux.react.uielements
import flux.stores.AllDocumentsStore
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import jsfacades.Mousetrap
import models.access.EntityAccess
import models.user.User

import scala.collection.immutable.Seq

private[app] final class Menu(implicit entityAccess: EntityAccess,
                              user: User,
                              clock: Clock,
                              i18n: I18n,
                              allDocumentsStore: AllDocumentsStore) {

  private val waitForFuture = new uielements.WaitForFuture[AllDocumentsStore.State]

  private val component = ScalaComponent
    .builder[Props](getClass.getSimpleName)
    .renderBackend[Backend]
    .componentWillMount(scope => scope.backend.configureKeyboardShortcuts(scope.props.router))
    .componentDidMount(scope =>
      LogExceptionsCallback {
        scope.props.router.currentPage match {
          // TODO: Restore
          //case page: Page.Search => {
          //  scope.backend.queryInputRef().setValue(page.query)
          //}
          case _ =>
        }
    })
    .componentWillReceiveProps(scope => scope.backend.configureKeyboardShortcuts(scope.nextProps.router))
    .build

  // **************** API ****************//
  def apply(router: RouterContext): VdomElement = {
    component(Props(router))
  }

  // **************** Private inner types ****************//
  private type State = Unit
  private class Backend(val $ : BackendScope[Props, State]) {
    val queryInputRef = uielements.input.TextInput.ref()

    def render(props: Props, state: State) = logExceptions {
      implicit val router = props.router
      def menuItem(label: String, page: Page, iconClass: String = null): VdomElement =
        router
          .anchorWithHrefTo(page)(
            ^^.ifThen(page == props.router.currentPage) { ^.className := "active" },
            ^.key := page.toString,
            <.i(^.className := Option(iconClass) getOrElse page.iconClass),
            " ",
            <.span(^.dangerouslySetInnerHtml := label)
          )

      <.ul(
        ^.className := "nav",
        ^.id := "side-menu",
        <.li(
          ^.className := "sidebar-search",
          <.form(
            <.div(
              ^.className := "input-group custom-search-form",
              uielements.input
                .TextInput(
                  ref = queryInputRef,
                  name = "query",
                  placeholder = i18n("app.search"),
                  classes = Seq("form-control")),
              <.span(
                ^.className := "input-group-btn",
                <.button(
                  ^.className := "btn btn-default",
                  ^.tpe := "submit",
                  ^.onClick ==> { (e: ReactEventFromInput) =>
                    LogExceptionsCallback {
                      e.preventDefault()

                      queryInputRef().value match {
                        case Some(query) =>
                        // TODO: Fix
                        //props.router.setPage(Page.Search(query))
                        case None =>
                      }
                    }
                  },
                  <.i(^.className := "fa fa-search")
                )
              )
            ))
        ),
        <.li(
          {
            for (document <- allDocumentsStore.state.allDocuments)
              yield menuItem(document.name, Page.DesktopTaskList(document.id))
          }.toVdomArray
        )
      )
    }

    def configureKeyboardShortcuts(implicit router: RouterContext): Callback = LogExceptionsCallback {
      def bind(shortcut: String, runnable: () => Unit): Unit = {
        Mousetrap.bindGlobal(shortcut, e => {
          e.preventDefault()
          runnable()
        })
      }
      def bindToPage(shortcut: String, page: Page): Unit =
        bind(shortcut, () => {
          router.setPage(page)
        })

      bind("shift+alt+f", () => queryInputRef().focus())
    }
  }

  private case class Props(router: RouterContext)
}
