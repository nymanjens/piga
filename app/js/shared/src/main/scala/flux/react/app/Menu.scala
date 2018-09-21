package flux.react.app

import common.I18n
import common.LoggingUtils.{LogExceptionsCallback, logExceptions}
import common.time.Clock
import flux.react.ReactVdomUtils.^^
import flux.react.router.{Page, RouterContext}
import flux.react.uielements
import flux.stores.StateStore
import flux.stores.document.AllDocumentsStore
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import jsfacades.Mousetrap
import models.access.EntityAccess
import models.document.DocumentEntity
import models.user.User

import scala.collection.immutable.Seq

private[app] final class Menu(implicit entityAccess: EntityAccess,
                              user: User,
                              clock: Clock,
                              i18n: I18n,
                              allDocumentsStore: AllDocumentsStore) {

  private val component = ScalaComponent
    .builder[Props](getClass.getSimpleName)
    .initialState(State(allDocuments = allDocumentsStore.state.allDocuments))
    .renderBackend[Backend]
    .componentWillMount(scope => scope.backend.willMount(scope.props, scope.state))
    .componentDidMount(scope => scope.backend.didMount(scope.props))
    .componentWillUnmount(scope => scope.backend.willUnmount())
    .componentWillReceiveProps(scope => scope.backend.configureKeyboardShortcuts(scope.nextProps.router))
    .build

  // **************** API ****************//
  def apply(router: RouterContext): VdomElement = {
    component(Props(router))
  }

  // **************** Private inner types ****************//
  private case class Props(router: RouterContext)
  private case class State(allDocuments: Seq[DocumentEntity])

  private class Backend(val $ : BackendScope[Props, State]) extends StateStore.Listener {
    val queryInputRef = uielements.input.TextInput.ref()

    def willMount(props: Props, state: State): Callback = LogExceptionsCallback {
      allDocumentsStore.register(this)
      $.modState(state => logExceptions(state.copy(allDocuments = allDocumentsStore.state.allDocuments)))
        .runNow()

      configureKeyboardShortcuts(props.router).runNow()
    }
    def didMount(props: Props): Callback = LogExceptionsCallback {
      props.router.currentPage match {
        // TODO: Restore
        //case page: Page.Search => {
        //  scope.backend.queryInputRef().setValue(page.query)
        //}
        case _ =>
      }
    }

    def willUnmount(): Callback = LogExceptionsCallback {
      allDocumentsStore.deregister(this)
    }

    override def onStateUpdate() = {
      $.modState(state => logExceptions(state.copy(allDocuments = allDocumentsStore.state.allDocuments)))
        .runNow()
    }

    def render(props: Props, state: State) = logExceptions {
      implicit val router = props.router
      def menuItem(label: String, page: Page, iconClass: String = null): VdomElement =
        router
          .anchorWithHrefTo(page)(
            ^^.ifThen(page == props.router.currentPage) { ^.className := "active" },
            // Add underscore to force rerender to fix bug when mouse is on current menu item
            ^.key := (page.toString + (if (page == props.router.currentPage) "_" else "")),
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
            for (document <- state.allDocuments)
              yield menuItem(document.name, Page.DesktopTaskList(document.id))
          }.toVdomArray
        ),
        <.li(
          menuItem(i18n("app.document-administration.html"), Page.DocumentAdministration)
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
      def goToAdjacentMenuItem(step: Int): Unit = {
        val allLeftMenuPages =
          allDocumentsStore.state.allDocuments.map(document => Page.DesktopTaskList(document.id)) :+
            Page.DocumentAdministration
        allLeftMenuPages.indexOf(router.currentPage) match {
          case -1 =>
          case i if 0 <= i + step && i + step < allLeftMenuPages.size =>
            router.setPage(allLeftMenuPages(i + step))
          case _ =>
        }
      }

      bind("shift+alt+f", () => queryInputRef().focus())
      bindToPage("shift+alt+d", Page.DocumentAdministration)
      bind("shift+alt+up", () => goToAdjacentMenuItem(step = -1))
      bind("shift+alt+down", () => goToAdjacentMenuItem(step = +1))
    }
  }
}
