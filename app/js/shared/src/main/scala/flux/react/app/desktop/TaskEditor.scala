package flux.react.app.desktop

import common.I18n
import common.LoggingUtils.{LogExceptionsCallback, logExceptions}
import flux.react.router.RouterContext
import japgolly.scalajs.react._
import japgolly.scalajs.react.raw.SyntheticKeyboardEvent
import japgolly.scalajs.react.vdom.PackageBase.VdomAttr
import japgolly.scalajs.react.vdom.html_<^._
import models.access.EntityAccess
import org.scalajs
import org.scalajs.dom
import org.scalajs.dom.{console, document}

import scala.scalajs.js

private[desktop] final class TaskEditor(implicit entityAccess: EntityAccess, i18n: I18n) {

  private val component = ScalaComponent
    .builder[Props](getClass.getSimpleName)
    .initialState(State())
    .renderBackend[Backend]
    .build

  // **************** API ****************//
  def apply()(implicit router: RouterContext): VdomElement = {
    component(Props(router))
  }

  // **************** Private inner types ****************//
  private case class Props(router: RouterContext)
  private case class State(lines: Seq[String] = Seq("Hello", "World!"))

  private class Backend($ : BackendScope[Props, State]) {
    def render(props: Props, state: State): VdomElement = logExceptions {
      implicit val router = props.router
      <.span(
        <.div(
          ^.contentEditable := true,
          VdomAttr("suppressContentEditableWarning") := true,
          ^.onInput ==> onChange,
          ^.onBlur ==> onChange,
          ^.onKeyDown ==> handleKeyDown,
          <.ul(
            (for ((line, i) <- state.lines.zipWithIndex)
              yield
                <.li(
                  ^.key := s"li-$i",
                  ^.id := s"teli-$i",
                  VdomAttr("num") := i,
                  line
                )).toVdomArray
          )
        ),
        <.br(),
        "----------------",
        <.br(),
        <.br(),
        <.br(),
        (for ((line, i) <- state.lines.zipWithIndex)
          yield <.div(^.key := s"line-$i", "- ", line)).toVdomArray
      )
    }

    private def onChange(event: ReactEventFromInput): Callback = LogExceptionsCallback {
      val sel: dom.raw.Selection = dom.window.getSelection()
      console.log("ONCHANGE EVENT", sel)
    }

    private def handleKeyDown(event: SyntheticKeyboardEvent[_]): Callback = LogExceptionsCallback {
      val eventKey = event.key
      if (eventKey.length == 1 && !event.ctrlKey) {
        val selection: dom.raw.Selection = dom.window.getSelection()
        val selectedLi = parentElement(selection.anchorNode)
        val selectedLineIndex = selectedLi.getAttribute("num").toInt

        event.preventDefault()

        $.modState(
          state =>
            replaceSelectionInState(
              state,
              replacement = eventKey,
              selectedLineIndex,
              lineOffset = selection.anchorOffset),
          setSelection(selectedLineIndex, lineOffset = selection.anchorOffset + 1)
        ).runNow()

      }
      // TODO: Handle selections
      // TODO: Handle newlines
      // TODO: Handle ctrl+v
    }

    private def replaceSelectionInState(state: State,
                                        replacement: String,
                                        selectedLineIndex: Int,
                                        lineOffset: Int): State = {
      val selectedLine = state.lines(selectedLineIndex)
      val updatedLine = insertInString(selectedLine, index = lineOffset, replacement)

      val updatedLines = state.lines.updated(selectedLineIndex, updatedLine)
      state.copy(lines = updatedLines)
    }

    private def setSelection(selectedLineIndex: Int, lineOffset: Int): Callback = LogExceptionsCallback {
      val selectedLine = dom.document.getElementById(s"teli-$selectedLineIndex")

      val range = dom.document.createRange()
      range.setStart(selectedLine.firstChild, lineOffset)

      val selection = dom.window.getSelection()
      selection.removeAllRanges()
      selection.addRange(range)
    }

    private def insertInString(s: String, index: Int, toInsert: String): String = {
      require(index >= 0, s"index = $index < 0")
      require(index <= s.length, s"index = $index > length = ${s.length}")
      val (before, after) = s.splitAt(index)
      before + toInsert + after
    }

    private def parentElement(node: dom.raw.Node): dom.raw.Element = {
      if (node.nodeType == dom.raw.Node.ELEMENT_NODE) {
        node.asInstanceOf[dom.raw.Element]
      } else {
        parentElement(node.parentNode)
      }
    }

    private def toContent(lines: Seq[String]): String = {
      s"<ul><li>${lines.mkString("</li><li>")}</li></ul>"
    }
  }
}
