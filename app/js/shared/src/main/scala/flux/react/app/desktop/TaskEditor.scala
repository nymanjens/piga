package flux.react.app.desktop

import common.DomNodeUtils._
import common.GuavaReplacement.Splitter
import common.{I18n, OrderToken}
import common.LoggingUtils.{LogExceptionsCallback, logExceptions}
import common.time.Clock
import flux.react.app.desktop.TaskSequence.{IndexedCursor, IndexedSelection}
import flux.react.router.RouterContext
import japgolly.scalajs.react._
import japgolly.scalajs.react.raw.SyntheticKeyboardEvent
import japgolly.scalajs.react.vdom.PackageBase.VdomAttr
import japgolly.scalajs.react.vdom.html_<^._
import jsfacades.escapeHtml
import models.access.EntityAccess
import org.scalajs.dom
import org.scalajs.dom.console

import scala.collection.immutable.Seq
import scala.scalajs.js

private[desktop] final class TaskEditor(implicit entityAccess: EntityAccess, i18n: I18n, clock: Clock) {

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
  private case class State(
      tasks: TaskSequence = new TaskSequence(
        Seq(
          Task.withRandomId(OrderToken.middleBetween(None, None), "Hel\nlo\n\n\n\n\nE<p>ND", indentation = 0),
          Task.withRandomId(OrderToken.middleBetween(None, None), "<indented>", indentation = 2),
          Task.withRandomId(
            OrderToken.middleBetween(Some(OrderToken.middleBetween(None, None)), None),
            "World!",
            indentation = 0)
        )))

  private class Backend($ : BackendScope[Props, State]) {

    private val editHistory: EditHistory = new EditHistory()

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
          ^.onCut ==> handleCut,
          ^.onCopy ==> handleCopy,
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
          yield
            <.div(^.key := s"task-$i", "- [", task.indentation, "]", contentToHtml(task.content))).toVdomArray
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
      val selection = IndexedCursor.tupleFromSelection(dom.window.getSelection())
      console
        .log(
          s"$name EVENT",
          event.nativeEvent,
          event.eventType,
          selection.toString
        )
    }

    private def handleCopy(event: ReactEventFromInput): Callback = logExceptions {
      event.preventDefault()

      modifyEventClipboardData(event)
      Callback.empty
    }

    private def handleCut(event: ReactEventFromInput): Callback = logExceptions {
      event.preventDefault()

      modifyEventClipboardData(event)

      val selection = IndexedCursor.tupleFromSelection(dom.window.getSelection())
      replaceSelectionInState(replacement = "", selection)
    }

    private def modifyEventClipboardData(event: ReactEventFromInput): Unit = {
      val IndexedSelection(start, end) = IndexedCursor.tupleFromSelection(dom.window.getSelection())
      val tasks = $.state.runNow().tasks

      if (start != end) {
        case class Subtask(task: Task, startOffset: Int, endOffset: Int) {
          def content: String = task.content.substring(startOffset, endOffset)
        }
        val substasks = tasks.zipWithIndex
          .filter { case (task, index) => start.seqIndex <= index && index <= end.seqIndex }
          .map {
            case (task, index) =>
              Subtask(
                task,
                startOffset = if (index == start.seqIndex) start.offsetInTask else 0,
                endOffset = if (index == end.seqIndex) end.offsetInTask else task.content.length)
          }
        val htmlText =
          "<ul><li>" +
            substasks
              .map(t => escapeHtml(t.content))
              .mkString("</li><li>")
              .replace("\n", "<br />") +
            "</li></ul>"
        val plainText = substasks.map(_.content).mkString("\n")

        event.nativeEvent.asInstanceOf[js.Dynamic].clipboardData.setData("text/html", htmlText)
        event.nativeEvent.asInstanceOf[js.Dynamic].clipboardData.setData("text/plain", plainText)
      }
    }

    private def handlePaste(event: ReactEventFromInput): Callback = logExceptions {
      val html = {
        val resultHolder = dom.document.createElement("span")
        resultHolder.innerHTML = getAnyClipboardString(event)
        resultHolder
      }

      val pastedText = {
        val resultBuilder = StringBuilder.newBuilder
        def addPastedText(node: dom.raw.Node, inListItem: Boolean): Unit = {
          asTextNode(node) match {
            case Some(textNode) =>
              resultBuilder.append(
                if (inListItem) textNode.wholeText else textNode.wholeText.replace('\n', TASK_DELIMITER))
            case None =>
          }
          if (nodeIsBr(node)) {
            resultBuilder.append(if (inListItem) '\n' else TASK_DELIMITER)
          }
          for (i <- 0 until node.childNodes.length) yield {
            addPastedText(node.childNodes.item(i), inListItem = inListItem || nodeIsLi(node))
          }
          if (nodeIsDiv(node) || nodeIsLi(node)) {
            resultBuilder.append(if (inListItem) '\n' else TASK_DELIMITER)
          }
        }
        addPastedText(html, inListItem = false)
        resultBuilder.toString.stripSuffix(TASK_DELIMITER.toString).stripSuffix("\n")
      }

      event.preventDefault()
      val selection = IndexedCursor.tupleFromSelection(dom.window.getSelection())
      replaceSelectionInState(replacement = pastedText, selection)
    }

    private def handleBeforeInput(event: ReactEventFromInput): Callback = LogExceptionsCallback {
      val selection = IndexedCursor.tupleFromSelection(dom.window.getSelection())
      console.log(s"onBeforeInput EVENT", event.nativeEvent, event.eventType, selection.toString)
      event.preventDefault()
    }

    private def handleKeyDown(event: SyntheticKeyboardEvent[_]): Callback = logExceptions {
      val selection = IndexedCursor.tupleFromSelection(dom.window.getSelection())
      val IndexedSelection(start, end) = selection
      implicit val tasks = $.state.runNow().tasks
      val shiftPressed = event.shiftKey
      val ctrlPressed = event.ctrlKey // TODO: Set to metaKey when Mac OS X

      event.key match {
        case eventKey if eventKey.length == 1 && !ctrlPressed =>
          event.preventDefault()
          replaceSelectionInState(replacement = eventKey, IndexedSelection(start, end))

        case "Enter" =>
          event.preventDefault()
          if (shiftPressed) {
            replaceSelectionInState(replacement = "\n", IndexedSelection(start, end))
          } else {
            replaceSelectionInState(replacement = TASK_DELIMITER.toString, IndexedSelection(start, end))
          }

        case "Backspace" =>
          event.preventDefault()
          if (start == end) {
            if (ctrlPressed) {
              replaceSelectionInState(replacement = "", IndexedSelection(start.minusWord, end))
            } else {
              replaceSelectionInState(replacement = "", IndexedSelection(start minusOffsetInSeq 1, end))
            }
          } else {
            replaceSelectionInState(replacement = "", IndexedSelection(start, end))
          }

        case "Delete" =>
          event.preventDefault()
          if (start == end) {
            if (ctrlPressed) {
              replaceSelectionInState(replacement = "", IndexedSelection(start, end.plusWord))
            } else {
              replaceSelectionInState(replacement = "", IndexedSelection(start, end plusOffsetInSeq 1))
            }
          } else {
            replaceSelectionInState(replacement = "", IndexedSelection(start, end))
          }

        case "Tab" =>
          event.preventDefault()
          indentSelectionInState(indentIncrease = if (shiftPressed) -1 else 1, selection)

        case "i" | "b" | "u" if ctrlPressed =>
          // Disable modifiers
          event.preventDefault()
          Callback.empty

        case "z" if ctrlPressed && !shiftPressed =>
          applyHistoryEdit(editHistory.undo())

        case "Z" if ctrlPressed && shiftPressed =>
          applyHistoryEdit(editHistory.redo())

        case "y" if ctrlPressed =>
          applyHistoryEdit(editHistory.redo())

        case _ =>
          Callback.empty
      }
    }

    private def applyHistoryEdit(maybeEdit: Option[EditHistory.Edit]): Callback = maybeEdit match {
      case None => Callback.empty
      case Some(edit) =>
        val oldTasks = $.state.runNow().tasks
        val newTasks = oldTasks.replaced(toReplace = edit.removedTasks, toAdd = edit.addedTasks)
        $.modState(
          _.copy(tasks = newTasks),
          setSelection(edit.selectionAfterEdit.attachToTasks(newTasks))
        )
    }

    private def replaceSelectionInState(replacement: String,
                                        selectionBeforeEdit: IndexedSelection): Callback = {
      val IndexedSelection(start, end) = selectionBeforeEdit
      val replacements = Splitter.on(TASK_DELIMITER).split(replacement)
      val oldTasks = $.state.runNow().tasks

      val newOrderTokens = {
        val previousTask = oldTasks.option(start.seqIndex - 1)
        val nextTask = oldTasks.option(end.seqIndex + 1)
        OrderToken.evenlyDistributedValuesBetween(
          numValues = replacements.length,
          lower = previousTask.map(_.orderToken),
          higher = nextTask.map(_.orderToken)
        )
      }
      val tasksToReplace = for (i <- start.seqIndex to end.seqIndex) yield oldTasks(i)
      val tasksToAdd =
        for (((replacementPart, newOrderToken), i) <- (replacements zip newOrderTokens).zipWithIndex)
          yield {
            def ifIndexOrEmpty(index: Int)(string: String): String = if (i == index) string else ""
            Task.withRandomId(
              orderToken = newOrderToken,
              ifIndexOrEmpty(0)(oldTasks(start.seqIndex).content.substring(0, start.offsetInTask)) +
                replacementPart +
                ifIndexOrEmpty(replacements.length - 1)(
                  oldTasks(end.seqIndex).content.substring(end.offsetInTask)),
              indentation = oldTasks(start.seqIndex).indentation
            )
          }

      replaceInStateWithHistory(
        tasksToReplace = tasksToReplace,
        tasksToAdd = tasksToAdd,
        selectionBeforeEdit = selectionBeforeEdit,
        selectionAfterEdit = IndexedSelection.collapsed(
          (start proceedNTasks (replacements.length - 1)) plusOffset replacements.last.length),
        replacementString = replacement
      )
    }

    private def indentSelectionInState(indentIncrease: Int, selection: IndexedSelection): Callback = {
      val oldTasks = $.state.runNow().tasks

      val IndexedSelection(start, end) = selection
      val tasksToReplace = for (i <- start.seqIndex to end.seqIndex) yield oldTasks(i)
      val tasksToAdd = tasksToReplace.map(
        task =>
          Task.withRandomId(
            orderToken = task.orderToken,
            content = task.content,
            indentation = zeroIfNegative(task.indentation + indentIncrease)))

      replaceInStateWithHistory(
        tasksToReplace = tasksToReplace,
        tasksToAdd = tasksToAdd,
        selectionBeforeEdit = selection,
        selectionAfterEdit = selection
      )
    }

    private def replaceInStateWithHistory(tasksToReplace: Seq[Task],
                                          tasksToAdd: Seq[Task],
                                          selectionBeforeEdit: IndexedSelection,
                                          selectionAfterEdit: IndexedSelection,
                                          replacementString: String = ""): Callback = {
      val oldTasks = $.state.runNow().tasks
      val newTasks = oldTasks.replaced(toReplace = tasksToReplace, toAdd = tasksToAdd)

      $.modState(
        _.copy(tasks = newTasks), {
          editHistory.addEdit(
            removedTasks = tasksToReplace,
            addedTasks = tasksToAdd,
            selectionBeforeEdit = selectionBeforeEdit.detach(oldTasks),
            selectionAfterEdit = selectionAfterEdit.detach(newTasks),
            replacementString = replacementString
          )
          setSelection(selectionAfterEdit)
        }
      )
    }

    private def setSelection(selection: IndexedSelection): Callback = LogExceptionsCallback {
      def getTaskElement(cursor: IndexedCursor): dom.raw.Element = {
        val task = dom.document.getElementById(s"teli-${cursor.seqIndex}")
        require(!js.isUndefined(task), s"Could not find task with index teli-${cursor.seqIndex}")
        task
      }
      def findCursorInDom(cursor: IndexedCursor)(func: (dom.raw.Node, Int) => Unit): Unit = {
        walkDepthFirstPreOrder(getTaskElement(cursor)).find {
          case NodeWithOffset(node, offsetSoFar, offsetAtEnd) =>
            if (offsetSoFar <= cursor.offsetInTask && cursor.offsetInTask <= offsetAtEnd) {
              func(node, cursor.offsetInTask - offsetSoFar)

              true
            } else {
              false
            }
        }
      }

      val IndexedSelection(start, end) = selection
      val resultRange = dom.document.createRange()
      findCursorInDom(start)(resultRange.setStart)
      findCursorInDom(end)(resultRange.setEnd)

      val windowSelection = dom.window.getSelection()
      windowSelection.removeAllRanges()
      windowSelection.addRange(resultRange)

      if (!elementIsFullyInView(getTaskElement(end))) {
        getTaskElement(end)
          .asInstanceOf[js.Dynamic]
          .scrollIntoView(js.Dynamic.literal(behavior = "instant", block = "nearest", inline = "nearest"))
      }
    }

    private def elementIsFullyInView(element: dom.raw.Element): Boolean = {
      val rect = element.getBoundingClientRect

      rect.top >= 0 &&
      rect.left >= 0 &&
      rect.bottom <= dom.window.innerHeight &&
      rect.right <= dom.window.innerWidth
    }

    private def getAnyClipboardString(event: ReactEventFromInput): String = {
      val htmlString =
        event.nativeEvent.asInstanceOf[js.Dynamic].clipboardData.getData("text/html").asInstanceOf[String]
      if (htmlString.nonEmpty) {
        htmlString
      } else {
        event.nativeEvent.asInstanceOf[js.Dynamic].clipboardData.getData("text/plain").asInstanceOf[String]
      }
    }

    private def zeroIfNegative(i: Int): Int = if (i < 0) 0 else i
  }
}
