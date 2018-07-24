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
import scala.collection.immutable.Seq

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

  private case class LineIndexWithOffset(lineIndex: Int, lineOffset: Int)
      extends Ordered[LineIndexWithOffset] {
    require(lineOffset >= 0)

    import scala.math.Ordered.orderingToOrdered

    override def compare(that: LineIndexWithOffset): Int =
      (this.lineIndex, this.lineOffset) compare ((that.lineIndex, that.lineOffset))

    def plusIndex(diff: Int): LineIndexWithOffset = LineIndexWithOffset(lineIndex + diff, lineOffset)
    def minusIndex(diff: Int): LineIndexWithOffset = plusIndex(-diff)
    def plusOffset(diff: Int): LineIndexWithOffset = LineIndexWithOffset(lineIndex, lineOffset + diff)
    def minusOffset(diff: Int): LineIndexWithOffset = plusOffset(-diff)

    def toEndOfLine(implicit state: State): LineIndexWithOffset =
      copy(lineOffset = state.lines(lineIndex).length)
  }
  private object LineIndexWithOffset {
    def tupleFromSelection(selection: dom.raw.Selection): (LineIndexWithOffset, LineIndexWithOffset) = {
      val anchor = LineIndexWithOffset.fromNode(selection.anchorNode, selection.anchorOffset)
      val focus = LineIndexWithOffset.fromNode(selection.focusNode, selection.focusOffset)
      if (anchor < focus) (anchor, focus) else (focus, anchor)
    }

    def fromNode(node: dom.raw.Node, offset: Int): LineIndexWithOffset =
      LineIndexWithOffset(lineIndex = parentElement(node).getAttribute("num").toInt, lineOffset = offset)

    private def parentElement(node: dom.raw.Node): dom.raw.Element = {
      if (node.nodeType == dom.raw.Node.ELEMENT_NODE) {
        node.asInstanceOf[dom.raw.Element]
      } else {
        parentElement(node.parentNode)
      }
    }
  }

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

    private def handleKeyDown(event: SyntheticKeyboardEvent[_]): Callback = logExceptions {
      val (start, end) = LineIndexWithOffset.tupleFromSelection(dom.window.getSelection())
      implicit val state = $.state.runNow()

      event.key match {
        case eventKey if eventKey.length == 1 && !event.ctrlKey =>
          event.preventDefault()
          replaceSelectionInState(replacement = eventKey, start, end)

        case "Enter" if !event.ctrlKey =>
          event.preventDefault()
          splitSelectionInState(start, end)

        case "Backspace" if !event.ctrlKey =>
          event.preventDefault()
          if (start == end) {
            if (start.lineOffset > 0) {
              replaceSelectionInState(replacement = "", start minusOffset 1, end)
            } else if (start.lineIndex > 0) {
              replaceSelectionInState(replacement = "", (start minusIndex 1).toEndOfLine, end)
            } else {
              // At start of document --> do nothing
              Callback.empty
            }
          } else { // selection is nonEmpty
            replaceSelectionInState(replacement = "", start, end)
          }

        case _ =>
          Callback.empty
      }
      // TODO: Handle delete
      // TODO: Handle ctrl + backspace / delete
      // TODO: Handle ctrl+enter
      // TODO: Handle ctrl+v
      // TODO: Handle ctrl+(shift+)z
      // TODO: Handle selection bounds outside editor
      // TODO: Disable ctrl+b, ctrl+u, ctrl+i
      // TODO: Handle tab to indent list
    }

    private def replaceSelectionInState(replacement: String,
                                        start: LineIndexWithOffset,
                                        end: LineIndexWithOffset): Callback = {
      $.modState(
        state =>
          if (start == end) {
            // Optimization
            val selectedLine = state.lines(start.lineIndex)
            val updatedLine = insertInString(selectedLine, index = start.lineOffset, replacement)
            state.copy(lines = state.lines.updated(start.lineIndex, updatedLine))
          } else {
            val updatedLine =
              state.lines(start.lineIndex).substring(0, start.lineOffset) +
                replacement +
                state.lines(end.lineIndex).substring(end.lineOffset)
            state.copy(lines = state.lines.zipWithIndex.flatMap {
              case (line, start.lineIndex)                                            => Some(updatedLine)
              case (line, index) if start.lineIndex < index && index <= end.lineIndex => None
              case (line, _)                                                          => Some(line)
            })
        },
        setSelection(start plusOffset replacement.length)
      )
    }

    private def splitSelectionInState(start: LineIndexWithOffset, end: LineIndexWithOffset): Callback = {
      $.modState(
        state => {
          val updatedStartLine = state.lines(start.lineIndex).substring(0, start.lineOffset)
          val updatedEndLine = state.lines(end.lineIndex).substring(end.lineOffset)

          state.copy(lines = state.lines.zipWithIndex.flatMap {
            case (line, start.lineIndex)                                            => Seq(updatedStartLine, updatedEndLine)
            case (line, index) if start.lineIndex < index && index <= end.lineIndex => None
            case (line, _)                                                          => Seq(line)
          })
        },
        setSelection(LineIndexWithOffset(start.lineIndex + 1, 0))
      )
    }

    private def setSelection(indexWithOffset: LineIndexWithOffset): Callback = LogExceptionsCallback {
      val selectedLine = dom.document.getElementById(s"teli-${indexWithOffset.lineIndex}")
      require(
        !js.isUndefined(selectedLine),
        s"Could not find line with index teli-${indexWithOffset.lineIndex}")

      val range = dom.document.createRange()
      if (indexWithOffset.lineOffset == 0) {
        range.setStart(selectedLine, 0)
      } else {
        range.setStart(selectedLine.firstChild, indexWithOffset.lineOffset)
      }

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

    private def toContent(lines: Seq[String]): String = {
      s"<ul><li>${lines.mkString("</li><li>")}</li></ul>"
    }
  }
}
