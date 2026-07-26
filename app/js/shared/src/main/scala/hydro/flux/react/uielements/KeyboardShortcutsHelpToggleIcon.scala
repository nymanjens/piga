package hydro.flux.react.uielements

import hydro.common.JsLoggingUtils.logExceptions
import hydro.flux.react.HydroReactComponent
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

import scala.scalajs.js

final class KeyboardShortcutsHelpToggleIcon extends HydroReactComponent {

  // **************** API ****************//
  def apply(): VdomElement = {
    component((): Unit)
  }

  // **************** Implementation of HydroReactComponent methods ****************//
  override protected val config = ComponentConfig(backendConstructor = new Backend(_), initialState = State())

  // **************** Implementation of HydroReactComponent types ****************//
  protected type Props = Unit
  protected case class State()

  protected class Backend($ : BackendScope[Props, State]) extends BackendBase($) {
    override def render(props: Props, state: State): VdomElement = logExceptions {
      Bootstrap.NavbarBrand()(
        ^.href := "#",
        ^.style := js.Dictionary("cursor" -> "pointer"),
        ^.onClick ==> { (e: ReactEvent) =>
          e.preventDefault()
          Callback(KeyboardShortcutsHelpOverlay.toggle())
        },
        Bootstrap.FontAwesomeIcon("question-circle")(^.title := "Keyboard shortcuts"),
      )
    }
  }
}
