package flux.react.app.desktop

import common.GuavaReplacement.Splitter
import common.{GuavaReplacement, I18n}
import common.LoggingUtils.{LogExceptionsCallback, logExceptions}
import flux.react.app.desktop.DomWalker.NodeWithOffset
import flux.react.router.RouterContext
import japgolly.scalajs.react._
import japgolly.scalajs.react.raw.SyntheticKeyboardEvent
import japgolly.scalajs.react.vdom.PackageBase.VdomAttr
import japgolly.scalajs.react.vdom.html_<^._
import models.access.EntityAccess
import org.scalajs.dom
import org.scalajs.dom.console

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
  private case class State(tasks: Seq[Task] = Seq(Task("Hel\nlo"), Task("World!")))

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
            (for ((task, i) <- state.tasks.zipWithIndex)
              yield
                <.li(
                  ^.key := s"li-$i",
                  ^.id := s"teli-$i",
                  VdomAttr("num") := i,
                  contentToHtml(task.content)
                )).toVdomArray
          )
        ),
        <.br(),
        "----------------",
        <.br(),
        <.br(),
        <.br(),
        (for ((task, i) <- state.tasks.zipWithIndex)
          yield <.div(^.key := s"task-$i", "- ", contentToHtml(task.content))).toVdomArray
      )
    }

    private def contentToHtml(content: String): VdomNode = {
      val contentWithSpaces = content.replace(" ", "\u00A0")

      def spaceIfNotFirst(index: Int): String = if (index == 0) "" else "*"
      def joinAsDiv(lines: Iterable[String]): VdomArray = {
        for ((line, index) <- lines.zipWithIndex) yield <.div(^.key := index, spaceIfNotFirst(index) + line)
      }.toVdomArray
      val lines = joinAsDiv(Splitter.on('\n').split(contentWithSpaces))
      lines
    }

    private def onChange(event: ReactEventFromInput): Callback = LogExceptionsCallback {
      val sel: dom.raw.Selection = dom.window.getSelection()
      console.log("ONCHANGE EVENT", sel)
    }

    private def handleKeyDown(event: SyntheticKeyboardEvent[_]): Callback = logExceptions {
      val (start, end) = TaskListCursor.tupleFromSelection(dom.window.getSelection())
      implicit val tasks = $.state.runNow().tasks
      val ctrlPressed = event.ctrlKey // TODO: Set to metaKey when Mac OS X

      event.key match {
        case eventKey if eventKey.length == 1 && !ctrlPressed =>
          event.preventDefault()
          replaceSelectionInState(replacement = eventKey, start, end)

        case "Enter" =>
          event.preventDefault()
          if (ctrlPressed) {
            replaceSelectionInState(replacement = "\n", start, end)
          } else {
            splitSelectionInState(start, end)
          }

        case "Backspace" =>
          event.preventDefault()
          if (start == end) {
            if (ctrlPressed) {
              replaceSelectionInState(replacement = "", start.minusWord, end)
            } else {
              replaceSelectionInState(replacement = "", start minusOffsetInList 1, end)
            }
          } else {
            replaceSelectionInState(replacement = "", start, end)
          }

        case "Delete" =>
          event.preventDefault()
          if (start == end) {
            if (ctrlPressed) {
              replaceSelectionInState(replacement = "", start, end.plusWord)
            } else {
              replaceSelectionInState(replacement = "", start, end plusOffsetInList 1)
            }
          } else {
            replaceSelectionInState(replacement = "", start, end)
          }

        case _ =>
          Callback.empty
      }
      // TODO: Handle ctrl+enter
      // TODO: Handle ctrl+(shift+)z
      // TODO: Handle ctrl+x, ctrl+v

      // TODO: Handle selection bounds outside editor
      // TODO: Disable ctrl+b, ctrl+u, ctrl+i
      // TODO: Handle tab to indent list
    }

    private def replaceSelectionInState(replacement: String,
                                        start: TaskListCursor,
                                        end: TaskListCursor): Callback = {
      $.modState(
        state =>
          if (start == end) {
            // Optimization
            val selectedTask = state.tasks(start.listIndex)
            val updatedTask =
              Task(insertInString(selectedTask.content, index = start.offsetInTask, replacement))
            state.copy(tasks = state.tasks.updated(start.listIndex, updatedTask))
          } else {
            val updatedTask = Task(
              state.tasks(start.listIndex).content.substring(0, start.offsetInTask) +
                replacement +
                state.tasks(end.listIndex).content.substring(end.offsetInTask))
            state.copy(tasks = state.tasks.zipWithIndex.flatMap {
              case (task, start.`listIndex`)                                          => Some(updatedTask)
              case (task, index) if start.listIndex < index && index <= end.listIndex => None
              case (task, _)                                                          => Some(task)
            })
        },
        setSelection(start plusOffset replacement.length)
      )
    }

    private def splitSelectionInState(start: TaskListCursor, end: TaskListCursor): Callback = {
      $.modState(
        state => {
          val updatedStartTask = Task(state.tasks(start.listIndex).content.substring(0, start.offsetInTask))
          val updatedEndTask = Task(state.tasks(end.listIndex).content.substring(end.offsetInTask))

          state.copy(tasks = state.tasks.zipWithIndex.flatMap {
            case (task, start.`listIndex`)                                          => Seq(updatedStartTask, updatedEndTask)
            case (task, index) if start.listIndex < index && index <= end.listIndex => None
            case (task, _)                                                          => Seq(task)
          })
        },
        setSelection(TaskListCursor(start.listIndex + 1, 0))
      )
    }

    private def setSelection(cursor: TaskListCursor): Callback = LogExceptionsCallback {
      val selectedTask = dom.document.getElementById(s"teli-${cursor.listIndex}")
      require(!js.isUndefined(selectedTask), s"Could not find task with index teli-${cursor.listIndex}")

      DomWalker.depthFirstPreOrder(selectedTask).find {
        case NodeWithOffset(node, offsetSoFar, offsetAtEnd) =>
          if (offsetSoFar <= cursor.offsetInTask && cursor.offsetInTask <= offsetAtEnd) {
            val range = dom.document.createRange()
            range.setStart(node, cursor.offsetInTask - offsetSoFar)
            val selection = dom.window.getSelection()
            selection.removeAllRanges()
            selection.addRange(range)

            true
          } else {
            false
          }
      }
    }

    private def insertInString(s: String, index: Int, toInsert: String): String = {
      require(index >= 0, s"index = $index < 0")
      require(index <= s.length, s"index = $index > length = ${s.length}")
      val (before, after) = s.splitAt(index)
      before + toInsert + after
    }

    private def toContent(tasks: Seq[String]): String = {
      s"<ul><li>${tasks.mkString("</li><li>")}</li></ul>"
    }
  }
}
