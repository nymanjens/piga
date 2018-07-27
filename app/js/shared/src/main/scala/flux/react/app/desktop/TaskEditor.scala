package flux.react.app.desktop

import common.GuavaReplacement.Splitter
import common.I18n
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

  /** Character that isn't expected to show up in normal text that is used to indicate the separation of two tasks */
  private val TASK_DELIMITER: Char = 23 // (End of Transmission Block)

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
  private case class State(tasks: Seq[Task] = Seq(Task("Hel\nlo\n\n\n\n\nEND"), Task("World!")))

  private class Backend($ : BackendScope[Props, State]) {
    def render(props: Props, state: State): VdomElement = logExceptions {
      implicit val router = props.router
      <.span(
        ^.className := "task-editor",
        <.div(
          ^.contentEditable := true,
          VdomAttr("suppressContentEditableWarning") := true,
          ^.onKeyDown ==> handleKeyDown,
          ^.onBeforeInput ==> handleBeforeInput,
          ^.onPaste ==> handlePaste,
          ^.onPasteCapture ==> handleEvent("onPasteCapture"),
          ^.onCut ==> handleEvent("onCut"),
          ^.onCopy ==> handleEvent("onCopy"),
          ^.onBlur ==> handleEvent("onBlur"),
          ^.onInput ==> handleEvent("onChange"),
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
          yield <.div(^.key := s"task-$i", "- ", <.pre(contentToHtml(task.content)))).toVdomArray
      )
    }

    private def contentToHtml(content: String): VdomNode = {
      // Fix for tailing newline issue. The last \n is seemingly ignored unless a non-empty element is trailing
      if (content.endsWith("\n")) {
        content + " "
      } else {
        content
      }
    }

    private def handleEvent(name: String)(event: ReactEventFromInput): Callback = LogExceptionsCallback {
      val (start, end) = TaskListCursor.tupleFromSelection(dom.window.getSelection())
      console
        .log(
          s"$name EVENT",
          event.nativeEvent,
          event.eventType,
          start.toString,
          end.toString
        )
    }

    private def handlePaste(event: ReactEventFromInput): Callback = logExceptions {
      val html = {
        val resultHolder = dom.document.createElement("span")
        resultHolder.innerHTML = {
          val htmlString =
            event.nativeEvent.asInstanceOf[js.Dynamic].clipboardData.getData("text/html").asInstanceOf[String]
          if (htmlString.nonEmpty) {
            htmlString
          } else {
            event.nativeEvent
              .asInstanceOf[js.Dynamic]
              .clipboardData
              .getData("text/plain")
              .asInstanceOf[String]
          }
        }
        resultHolder
      }

      val pastedText = {
        val resultBuilder = StringBuilder.newBuilder
        def addPastedText(node: dom.raw.Node, inListItem: Boolean): Unit = {
          def isElement(tagName: String): Boolean =
            node.nodeType == dom.raw.Node.ELEMENT_NODE && node
              .asInstanceOf[dom.raw.Element]
              .tagName == tagName
          if (node.nodeType == dom.raw.Node.TEXT_NODE) {
            resultBuilder.append(
              if (inListItem) {
                node.asInstanceOf[dom.raw.Text].wholeText
              } else {
                node.asInstanceOf[dom.raw.Text].wholeText.replace('\n', TASK_DELIMITER)
              }
            )
          }
          if (isElement("BR")) {
            resultBuilder.append(if (inListItem) '\n' else TASK_DELIMITER)
          }
          for (i <- 0 until node.childNodes.length) yield {
            addPastedText(node.childNodes.item(i), inListItem = inListItem || isElement("LI"))
          }
          if (isElement("DIV") || isElement("LI")) {
            resultBuilder.append(if (inListItem) '\n' else TASK_DELIMITER)
          }
        }
        addPastedText(html, inListItem = false)
        resultBuilder.toString.stripSuffix(TASK_DELIMITER.toString).stripSuffix("\n")
      }

      event.preventDefault()
      val (start, end) = TaskListCursor.tupleFromSelection(dom.window.getSelection())
      replaceSelectionInState(replacement = pastedText, start, end)
    }

    private def handleBeforeInput(event: ReactEventFromInput): Callback = LogExceptionsCallback {
      val (start, end) = TaskListCursor.tupleFromSelection(dom.window.getSelection())
      console
        .log(s"onBeforeInput EVENT", event.nativeEvent, event.eventType, start.toString, end.toString)
      event.preventDefault()
    }

    private def handleKeyDown(event: SyntheticKeyboardEvent[_]): Callback = logExceptions {
      val (start, end) = TaskListCursor.tupleFromSelection(dom.window.getSelection())
      implicit val tasks = $.state.runNow().tasks
      val shiftPressed = event.shiftKey
      val ctrlPressed = event.ctrlKey // TODO: Set to metaKey when Mac OS X

      event.key match {
        case eventKey if eventKey.length == 1 && !ctrlPressed =>
          event.preventDefault()
          replaceSelectionInState(replacement = eventKey, start, end)

        case "Enter" =>
          event.preventDefault()
          if (shiftPressed) {
            replaceSelectionInState(replacement = "\n", start, end)
          } else {
            replaceSelectionInState(replacement = TASK_DELIMITER.toString, start, end)
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
    }

    private def replaceSelectionInState(replacement: String,
                                        start: TaskListCursor,
                                        end: TaskListCursor): Callback = {
      val replacements = Splitter.on(TASK_DELIMITER).split(replacement)

      $.modState(
        state =>
          if (start == end && replacements.length == 1) {
            // Optimization
            val selectedTask = state.tasks(start.listIndex)
            val updatedTask =
              Task(insertInString(selectedTask.content, index = start.offsetInTask, replacement))
            state.copy(tasks = state.tasks.updated(start.listIndex, updatedTask))
          } else {
            val updatedTasks = for ((replacementPart, i) <- replacements.zipWithIndex)
              yield {
                def ifIndexOrEmpty(index: Int)(string: String): String = if (i == index) string else ""
                Task(
                  ifIndexOrEmpty(0)(state.tasks(start.listIndex).content.substring(0, start.offsetInTask)) +
                    replacementPart +
                    ifIndexOrEmpty(replacements.length - 1)(
                      state.tasks(end.listIndex).content.substring(end.offsetInTask))
                )
              }
            state.copy(tasks = state.tasks.zipWithIndex.flatMap {
              case (task, start.`listIndex`)                                          => updatedTasks
              case (task, index) if start.listIndex < index && index <= end.listIndex => Seq()
              case (task, _)                                                          => Seq(task)
            })
        },
        setSelection((start proceedNTasks (replacements.length - 1)) plusOffset replacements.last.length)
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
  }
}
