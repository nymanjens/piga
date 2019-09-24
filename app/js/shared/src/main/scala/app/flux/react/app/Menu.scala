package app.flux.react.app

import app.flux.router.AppPages
import app.flux.stores.document.AllDocumentsStore
import app.models.document.DocumentEntity
import hydro.common.I18n
import hydro.flux.react.uielements.SbadminMenu
import hydro.flux.react.uielements.SbadminMenu.MenuItem
import hydro.flux.react.HydroReactComponent
import hydro.flux.router.RouterContext
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

import scala.collection.immutable.Seq

private[app] final class Menu(
    implicit i18n: I18n,
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
  protected case class State(allDocuments: Seq[DocumentEntity] = allDocumentsStore.state.allDocuments)

  protected class Backend($ : BackendScope[Props, State]) extends BackendBase($) {

    override def render(props: Props, state: State): VdomElement = {
      sbadminMenu(
        menuItems = Seq(
          for (document <- state.allDocuments)
            yield MenuItem(document.name, AppPages.TaskList(document.id)),
          Seq(
            MenuItem(
              i18n("app.document-administration.html"),
              AppPages.DocumentAdministration,
              shortcuts = Seq("shift+alt+d"))
          )
        ),
        enableSearch = false,
        router = props.router
      )
    }
  }
}
