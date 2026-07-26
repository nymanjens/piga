package app.flux.react.app

import hydro.flux.react.uielements.SbadminLayout
import hydro.flux.router.RouterContext
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

import scala.collection.immutable.Seq

final class Layout(implicit
    menu: Menu,
    sbadminLayout: SbadminLayout,
    keyboardShortcutsHelpOverlay: hydro.flux.react.uielements.KeyboardShortcutsHelpOverlay,
) {

  private val component = ScalaComponent
    .builder[Props](getClass.getSimpleName)
    .renderPC { (_, props, children) =>
      implicit val router = props.router
      sbadminLayout(
        title = "Task Keeper",
        leftMenu = menu(),
        pageContent = <.span(children),
        extraFooter = Seq(
          keyboardShortcutsHelpOverlay(
            Seq(
              "Basics" -> Seq(
                "Tab / Shift + Tab" -> "Increase/decrease current indentation",
                "Ctrl + I" -> "Toggle 'italic'",
                "Ctrl + B" -> "Toggle 'bold'",
                "Ctrl + ` or Alt + Shift + 1" -> "Toggle 'code font'",
                "Alt + Shift + 3" -> "Toggle 'highlight'",
                "Alt + Shift + 5" -> "Toggle 'strikethrough'",
                "Ctrl + \\" -> "Reset formatting",
                "Shift + Alt + /" -> "Show this help",
              ),
              "Special actions" -> Seq(
                "Ctrl + K" -> "Create or edit a link",
                "Alt + Shift + T" -> "Create or edit a tag",
                "Ctrl + P" -> "Open \"Go to file\" dialog",
                "Ctrl + Plus / Minus" -> "Expand / collapse current task",
                "Alt + Shift + 4" -> "Mark as 'done'",
                "Alt + Shift + D" -> "Queue task to be added later",
              ),
              "Power user" -> Seq(
                "Ctrl + Enter" -> "Open selected link",
                "Ctrl + Shift + C / X" -> "Copy / cut selected task and its children",
                "Alt + Shift + M" -> "Copy selected task and its children as Markdown",
                "Alt + Up" -> "Swap current task with the previous task",
                "Alt + Down" -> "Swap current task with the next task",
                "Ctrl + Shift + P" -> "Go to the parent task",
                "Ctrl + D" -> "Delete current task",
                "Ctrl + Shift + B" -> "Duplicate current task",
                "Ctrl + Shift + Delete" -> "Delete the remainder of the line after the cursor",
                "Shift + Alt + Up" -> "Switch to previous document",
                "Shift + Alt + Down" -> "Switch to next document",
              ),
              "Power user: Change casing" -> Seq(
                "Ctrl + Alt + U" -> "Convert selection to uppercase",
                "Ctrl + Alt + L" -> "Convert selection to lowercase",
                "Alt + Shift + L" -> "Convert selection to CamelCase",
                "Alt + Shift + K" -> "Convert selection to snake_case",
                "Alt + Shift + H" -> "Convert selection to dash-case",
              ),
              "Power user: Selection" -> Seq(
                "Ctrl + J" -> "Select current task",
                "Ctrl + M" -> "Select current word",
                "Ctrl + Shift + M" -> "Select current quoted sentence",
                "Ctrl + Q" -> "Go to the last edit",
                "Ctrl + G / Ctrl + Shift + G" -> "Find next / previous occurrence of selected text",
              ),
            )
          )
        ),
      )
    }
    .build

  // **************** API ****************//
  def apply(router: RouterContext)(children: VdomNode*): VdomElement = {
    component(Props(router))(children: _*)
  }

  // **************** Private inner types ****************//
  private case class Props(router: RouterContext)
}
