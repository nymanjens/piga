package hydro.flux.react.uielements

import hydro.flux.react.HydroReactComponent
import hydro.jsfacades.Mousetrap
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

import scala.collection.immutable.Seq
import scala.scalajs.js

final class KeyboardShortcutsHelpOverlay extends HydroReactComponent {

  // **************** API ****************//
  def apply(shortcuts: Seq[(String, Seq[(String, String)])]): VdomElement = {
    component(Props(shortcuts))
  }

  // **************** Implementation of HydroReactComponent methods ****************//
  override protected val config = ComponentConfig(backendConstructor = new Backend(_), initialState = State())

  // **************** Implementation of HydroReactComponent types ****************//
  protected case class Props(shortcuts: Seq[(String, Seq[(String, String)])])
  protected case class State(isVisible: Boolean = false)

  protected class Backend($ : BackendScope[Props, State]) extends BackendBase($) with WillMount {
    var isVisibleCurrently: Boolean = false

    override def willMount(props: Props, state: State): Callback = Callback {
      KeyboardShortcutsHelpOverlay.registerToggleCallback(() => {
        isVisibleCurrently = !isVisibleCurrently
        $.modState(s => s.copy(isVisible = isVisibleCurrently)).runNow()
      })

      Mousetrap.bindGlobal(
        "shift+alt+/",
        e => {
          e.preventDefault()
          isVisibleCurrently = !isVisibleCurrently
          $.modState(s => s.copy(isVisible = isVisibleCurrently)).runNow()
        },
      )
      Mousetrap.bindGlobal(
        "esc",
        e => {
          if (isVisibleCurrently) {
            e.preventDefault()
            hideOverlay().runNow()
          }
        },
      )
    }

    override def render(props: Props, state: State): VdomElement = {
      if (!state.isVisible) {
        <.span()
      } else {
        <.div(
          <.div(
            ^.className := "modal-backdrop fade in",
            ^.style := js.Dictionary("zIndex" -> 1040),
            ^.onClick --> hideOverlay(),
          ),
          <.div(
            ^.className := "modal fade in",
            ^.style := js.Dictionary("display" -> "block", "zIndex" -> 1050),
            ^.tabIndex := -1,
            <.div(
              ^.className := "modal-dialog",
              ^.style := js.Dictionary("width" -> "780px", "maxWidth" -> "95vw"),
              <.div(
                ^.className := "modal-content",
                <.div(
                  ^.className := "modal-header",
                  <.button(
                    ^.tpe := "button",
                    ^.className := "close",
                    ^.onClick --> hideOverlay(),
                    <.span("×"),
                  ),
                  <.h4(^.className := "modal-title", "Keyboard Shortcuts"),
                ),
                <.div(
                  ^.className := "modal-body",
                  ^.style := js.Dictionary("maxHeight" -> "calc(100vh - 150px)", "overflowY" -> "auto"),
                  <.table(
                    ^.className := "table table-bordered",
                    ^.style := js.Dictionary("marginBottom" -> "0"),
                    <.tbody(
                      props.shortcuts.zipWithIndex.flatMap { case ((category, categoryShortcuts), catIdx) =>
                        Seq(
                          <.tr(
                            ^.key := s"cat-$catIdx",
                            <.td(^.colSpan := 2, ^.className := "active", <.strong(category)),
                          )
                        ) ++
                          categoryShortcuts.zipWithIndex.map {
                            case ((shortcutString, description), shortIdx) =>
                              <.tr(
                                ^.key := s"short-$catIdx-$shortIdx",
                                <.td(renderShortcut(shortcutString)),
                                <.td(description),
                              )
                          }
                      }.toVdomArray
                    ),
                  ),
                ),
              ),
            ),
          ),
        )
      }
    }

    private def hideOverlay(): Callback = {
      isVisibleCurrently = false
      $.modState(_.copy(isVisible = false))
    }

    private def renderShortcut(shortcutString: String): VdomElement = {
      val tokens = shortcutString.split(" ")
      <.span(
        tokens.zipWithIndex.map { case (token, index) =>
          if (token == "+" || token == "/" || token == "or") {
            <.span(^.key := s"token-$index", s" $token "): VdomNode
          } else {
            Kbd(token)(^.key := s"token-$index"): VdomNode
          }
        }.toVdomArray
      )
    }

    private def Kbd(text: String): VdomTag = {
      <.kbd(
        ^.style := js.Dictionary(
          "backgroundColor" -> "#f8f9fa",
          "color" -> "#212529",
          "border" -> "1px solid #adb5bd",
          "fontSize" -> "100%",
        ),
        text,
      )
    }
  }
}

object KeyboardShortcutsHelpOverlay {
  private var toggleCallback: Option[() => Unit] = None

  def toggle(): Unit = toggleCallback.foreach(_())

  private[uielements] def registerToggleCallback(cb: () => Unit): Unit = {
    toggleCallback = Some(cb)
  }
}
