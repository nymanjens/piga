package flux.react.app.desktop

import common.DomNodeUtils._
import common.GuavaReplacement.Splitter
import common.LoggingUtils.{LogExceptionsCallback, logExceptions}
import common.ScalaUtils.visibleForTesting
import common.time.Clock
import common.{I18n, OrderToken}
import flux.react.app.desktop.Task.MarkupTag
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
import scala.collection.mutable
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
          Task.withRandomId(
            OrderToken.middleBetween(None, None),
            Seq(MarkupTag.Bold(Seq(MarkupTag.Text("Hello"))), MarkupTag.Text("\nmy")),
            indentation = 0),
          Task.withRandomId(
            OrderToken.middleBetween(None, None),
            Seq(MarkupTag.Text("<indented>")),
            indentation = 2),
          Task.withRandomId(
            OrderToken.middleBetween(Some(OrderToken.middleBetween(None, None)), None),
            Seq(MarkupTag.Text("world")),
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
                  ^.style := js.Dictionary("marginLeft" -> s"${task.indentation * 30}px"),
                  VdomAttr("num") := i,
                  contentToHtml(task.contentTags)
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
            <.div(^.key := s"task-$i", "- [", task.indentation, "]", contentToHtml(task.contentTags))).toVdomArray
      )
    }

    private def contentToHtml(content: Seq[MarkupTag]): VdomNode = {
      def toVdomNode(x: VdomNode): VdomNode = x

      def innerContentToHtml(content: Seq[MarkupTag],
                             addSpaceAfterTrailingNewline: Boolean = false): VdomNode =
        content.map {
          case MarkupTag.Text(text) =>
            // Fix for tailing newline issue. The last \n is seemingly ignored unless a non-empty element is trailing
            toVdomNode(
              if (addSpaceAfterTrailingNewline && text.endsWith("\n")) text + " " else text
            )

          case MarkupTag.Bold(children) => toVdomNode(<.b(innerContentToHtml(children)))
        }.toVdomArray

      innerContentToHtml(content, addSpaceAfterTrailingNewline = true)
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
      replaceSelectionInState(replacement = Replacement.empty, selection)
    }

    private def modifyEventClipboardData(event: ReactEventFromInput): Unit = {
      val selection = IndexedCursor.tupleFromSelection(dom.window.getSelection())
      val tasks = $.state.runNow().tasks

      if (selection.start != selection.end) {
        val ClipboardData(htmlText, plainText) = convertToClipboardData(tasks, selection)

        event.nativeEvent.asInstanceOf[js.Dynamic].clipboardData.setData("text/html", htmlText)
        event.nativeEvent.asInstanceOf[js.Dynamic].clipboardData.setData("text/plain", plainText)
      }
    }

    private def handlePaste(event: ReactEventFromInput): Callback = logExceptions {
      event.preventDefault()
      val selection = IndexedCursor.tupleFromSelection(dom.window.getSelection())
      replaceSelectionInState(
        replacement = clipboardStringToReplacement(getAnyClipboardString(event)),
        selection)
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
          replaceSelectionInState(
            replacement = Replacement.fromString(eventKey),
            IndexedSelection(start, end))

        case "Enter" =>
          event.preventDefault()
          if (shiftPressed) {
            replaceSelectionInState(replacement = Replacement.fromString("\n"), IndexedSelection(start, end))
          } else {
            replaceSelectionInState(replacement = Replacement.newEmptyTask(), IndexedSelection(start, end))
          }

        case "Backspace" =>
          event.preventDefault()
          if (start == end) {
            if (ctrlPressed) {
              replaceSelectionInState(replacement = Replacement.empty, IndexedSelection(start.minusWord, end))
            } else {
              replaceSelectionInState(
                replacement = Replacement.empty,
                IndexedSelection(start minusOffsetInSeq 1, end))
            }
          } else {
            replaceSelectionInState(replacement = Replacement.empty, IndexedSelection(start, end))
          }

        case "Delete" =>
          event.preventDefault()
          if (start == end) {
            if (ctrlPressed) {
              replaceSelectionInState(replacement = Replacement.empty, IndexedSelection(start, end.plusWord))
            } else {
              replaceSelectionInState(
                replacement = Replacement.empty,
                IndexedSelection(start, end plusOffsetInSeq 1))
            }
          } else {
            replaceSelectionInState(replacement = Replacement.empty, IndexedSelection(start, end))
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

    private def replaceSelectionInState(replacement: Replacement,
                                        selectionBeforeEdit: IndexedSelection): Callback = {
      val IndexedSelection(start, end) = selectionBeforeEdit
      val oldTasks = $.state.runNow().tasks

      val newOrderTokens = {
        val previousTask = oldTasks.option(start.seqIndex - 1)
        val nextTask = oldTasks.option(end.seqIndex + 1)
        OrderToken.evenlyDistributedValuesBetween(
          numValues = replacement.parts.length,
          lower = previousTask.map(_.orderToken),
          higher = nextTask.map(_.orderToken)
        )
      }
      val baseIndentation = oldTasks(start.seqIndex).indentation
      val tasksToReplace = for (i <- start.seqIndex to end.seqIndex) yield oldTasks(i)
      val tasksToAdd =
        for (((replacementPart, newOrderToken), i) <- (replacement.parts zip newOrderTokens).zipWithIndex)
          yield {
            def ifIndexOrEmpty(index: Int)(tags: Seq[MarkupTag]): Seq[MarkupTag] =
              if (i == index) tags else Seq()
            Task.withRandomId(
              orderToken = newOrderToken,
              contentTags = ifIndexOrEmpty(0)(
                MarkupTag.sub(oldTasks(start.seqIndex).contentTags, 0, start.offsetInTask)) ++
                replacementPart.contentTags ++
                ifIndexOrEmpty(replacement.parts.length - 1)(
                  MarkupTag.sub(oldTasks(end.seqIndex).contentTags, end.offsetInTask)),
              indentation = baseIndentation + replacementPart.indentationRelativeToCurrent
            )
          }

      replaceInStateWithHistory(
        tasksToReplace = tasksToReplace,
        tasksToAdd = tasksToAdd,
        selectionBeforeEdit = selectionBeforeEdit,
        selectionAfterEdit = IndexedSelection.collapsed(
          (start proceedNTasks (replacement.parts.length - 1)) plusOffset replacement.parts.last.contentString.length),
        replacementString = replacement.contentString
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
            contentTags = task.contentTags,
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
  }

  // **************** Helper classes and methods **************** //
  @visibleForTesting private[desktop] case class Replacement(parts: Seq[Replacement.Part]) {
    def contentString: String = parts.map(_.contentString).mkString
  }
  @visibleForTesting private[desktop] object Replacement {
    def create(firstPartContent: Seq[MarkupTag], otherParts: Part*): Replacement =
      Replacement(Part(firstPartContent, indentationRelativeToCurrent = 0) :: List(otherParts: _*))
    def fromString(string: String): Replacement = Replacement.create(Seq(MarkupTag.Text(string)))
    def newEmptyTask(indentationRelativeToCurrent: Int = 0): Replacement =
      Replacement.create(
        Seq(MarkupTag.Text("")),
        Part(
          contentTags = Seq(MarkupTag.Text("")),
          indentationRelativeToCurrent = indentationRelativeToCurrent))
    def empty: Replacement = Replacement.create(Seq(MarkupTag.Text("")))

    case class Part(contentTags: Seq[MarkupTag], indentationRelativeToCurrent: Int) {
      def contentString: String = MarkupTag.contentString(contentTags)
    }
  }
  @visibleForTesting private[desktop] case class ClipboardData(htmlText: String, plainText: String)
  @visibleForTesting private[desktop] def convertToClipboardData(
      tasks: TaskSequence,
      selection: IndexedSelection): ClipboardData = {
    case class Subtask(task: Task, startOffset: Int, endOffset: Int) {
      // TODO: Make content MarkupTag
      def content: String = task.contentString.substring(startOffset, endOffset)
      def indentation: Int = task.indentation
    }

    val IndexedSelection(start, end) = selection
    val subtasks = tasks.zipWithIndex
      .filter { case (task, index) => start.seqIndex <= index && index <= end.seqIndex }
      .map {
        case (task, index) =>
          Subtask(
            task,
            startOffset = if (index == start.seqIndex) start.offsetInTask else 0,
            endOffset = if (index == end.seqIndex) end.offsetInTask else task.contentString.length)
      }
    ClipboardData(
      htmlText = {
        val resultBuilder = StringBuilder.newBuilder
        val baseIndentation = subtasks.map(_.indentation).min - 1
        var lastIndentation = baseIndentation
        for (subtask <- subtasks) {
          for (i <- lastIndentation until subtask.indentation) {
            resultBuilder.append("<ul>")
          }
          for (i <- subtask.task.indentation until lastIndentation) {
            resultBuilder.append("</ul>")
          }
          resultBuilder.append("<li>")
          resultBuilder.append(escapeHtml(subtask.content).replace("\n", "<br />"))
          resultBuilder.append("</li>")
          lastIndentation = subtask.indentation
        }
        for (i <- baseIndentation until lastIndentation) {
          resultBuilder.append("</ul>")
        }
        resultBuilder.toString
      },
      plainText = subtasks.map(_.content).mkString("\n")
    )
  }

  @visibleForTesting private[desktop] def clipboardStringToReplacement(
      clipboardString: String): Replacement = {
    val html = {
      val resultHolder = dom.document.createElement("span")
      resultHolder.innerHTML = clipboardString
      resultHolder
    }

    val partsBuilder = mutable.Buffer[Replacement.Part]()
    var nextContent = ""
    var nextRelativeIndentation = -1

    def addNextPart(): Unit = {
      partsBuilder.append(Replacement.Part(Seq(MarkupTag.Text(nextContent.trim)), zeroIfNegative(nextRelativeIndentation)))
      nextContent = ""
    }
    def addPastedText(node: dom.raw.Node, inListItem: Boolean): Unit = {
      asTextNode(node) match {
        case Some(textNode) =>
          if (inListItem) {
            nextContent += textNode.wholeText
          } else {
            for ((line, i) <- Splitter.on('\n').split(textNode.wholeText).zipWithIndex) {
              if (i != 0) {
                addNextPart()
              }
              nextContent += line
            }
          }
        case None =>
      }
      if (nodeIsBr(node)) {
        if (inListItem) {
          nextContent += '\n'
        } else {
          addNextPart()
        }
      }
      if (nodeIsList(node)) {
        nextRelativeIndentation += 1
      }

      for (i <- 0 until node.childNodes.length) yield {
        addPastedText(node.childNodes.item(i), inListItem = inListItem || nodeIsLi(node))
      }

      // handle tag closings
      if (nodeIsDiv(node) || nodeIsP(node)) {
        if (inListItem) {
          nextContent += '\n'
        } else {
          addNextPart()
        }
      }
      if (nodeIsLi(node)) {
        addNextPart()
      }
      if (nodeIsList(node)) {
        nextRelativeIndentation -= 1
      }
    }

    addPastedText(html, inListItem = false)
    if (nextContent.nonEmpty) {
      addNextPart()
    }
    Replacement(partsBuilder.toVector)
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
