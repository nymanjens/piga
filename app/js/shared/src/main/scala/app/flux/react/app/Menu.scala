package app.flux.react.app

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.async.Async.async
import scala.async.Async.await
import app.common.document.UserDocument
import app.flux.react.uielements.SelectPrompt
import app.flux.router.AppPages
import app.flux.stores.document.AllDocumentsStore
import hydro.common.I18n
import hydro.flux.react.uielements.SbadminMenu
import hydro.flux.react.uielements.SbadminMenu.MenuItem
import hydro.flux.react.HydroReactComponent
import hydro.flux.router.RouterContext
import hydro.jsfacades.Mousetrap
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

import scala.collection.immutable.Seq

private[app] final class Menu(implicit
    i18n: I18n,
    allDocumentsStore: AllDocumentsStore,
    sbadminMenu: SbadminMenu,
) extends HydroReactComponent {

  // **************** API ****************//
  def apply()(implicit router: RouterContext): VdomElement = {
    component(Props(router))
  }

  // **************** Implementation of HydroReactComponent methods ****************//
  override protected val config = ComponentConfig(backendConstructor = new Backend(_), initialState = State())
    .withStateStoresDependency(allDocumentsStore, _.copy(allDocuments = allDocumentsStore.state.allDocuments))

  // **************** Implementation of HydroReactComponent types ****************//
  protected case class Props(router: RouterContext)
  protected case class State(allDocuments: Seq[UserDocument] = allDocumentsStore.state.allDocuments)

  protected class Backend($ : BackendScope[Props, State]) extends BackendBase($) {

    override def render(props: Props, state: State): VdomElement = {
      sbadminMenu(
        menuItems = Seq(
          for (document <- state.allDocuments)
            yield MenuItem(document.name, AppPages.TaskList(document.documentId)),
          Seq(
            MenuItem(
              i18n("app.document-administration.html"),
              AppPages.DocumentAdministration,
              shortcuts = Seq("shift+alt+d"),
            )
          ),
        ),
        enableSearch = false,
        router = props.router,
        configureAdditionalKeyboardShortcuts = () => configureAdditionalKeyboardShortcuts(props.router),
      )
    }

    private def configureAdditionalKeyboardShortcuts(router: RouterContext): Unit = {
      def bind(shortcut: String, runnable: () => Unit): Unit = {
        Mousetrap.bind(
          shortcut,
          e => {
            e.preventDefault()
            runnable()
          },
        )
      }
      def bindGlobal(shortcut: String, runnable: () => Unit): Unit = {
        Mousetrap.bindGlobal(
          shortcut,
          e => {
            e.preventDefault()
            runnable()
          },
        )
      }

      bindGlobal(
        "ctrl+p",
        () =>
          async {
            val answer = await(
              SelectPrompt.choose(
                title = "Go to file",
                optionsIdToName = allDocumentsStore.state.allDocuments.map(d => (d.documentId, d.name)).toMap,
              )
            )

            answer match {
              case None             => // do nothing
              case Some(documentId) => router.setPage(AppPages.TaskList(documentId))
            }
          },
      )
    }
  }
}
