package flux.react.app.desktop

import common.DomNodeUtils._
import common.GuavaReplacement.Splitter
import common.LoggingUtils.{LogExceptionsCallback, logExceptions}
import common.ScalaUtils.visibleForTesting
import common.time.Clock
import common.{I18n, OrderToken}
import models.document.TextWithMarkup.Formatting
import models.document.Document.{IndexedCursor, IndexedSelection}
import flux.react.router.RouterContext
import flux.stores.document.DocumentStore
import japgolly.scalajs.react._
import japgolly.scalajs.react.raw.SyntheticKeyboardEvent
import japgolly.scalajs.react.vdom.PackageBase.VdomAttr
import japgolly.scalajs.react.vdom.html_<^._
import jsfacades.escapeHtml
import models.access.EntityAccess
import models.document.{Document, Task, TextWithMarkup}
import org.scalajs.dom
import org.scalajs.dom.console

import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.scalajs.js

private[desktop] final class TaskEditor(implicit entityAccess: EntityAccess, i18n: I18n, clock: Clock) {

  private val component = ScalaComponent
    .builder[Props](getClass.getSimpleName)
    .initialStateFromProps(props => State(document = props.documentStore.state.document))
    .renderBackend[Backend]
    .build

  // **************** API ****************//
  def apply(documentStore: DocumentStore)(implicit router: RouterContext): VdomElement = {
    component(Props(documentStore, router))
  }

  // **************** Private inner types ****************//
  private case class Props(documentStore: DocumentStore, router: RouterContext)
  private case class State(document: Document)

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
          ^.onPaste ==> handlePaste,
          ^.onCut ==> handleCut,
          ^.onCopy ==> handleCopy,
          ^.onInput ==> handleEvent("onChange"),
          ^.onBeforeInput ==> handleEvent("onBeforeInput"),
          <.ul(
            (for ((task, i) <- state.document.tasks.zipWithIndex)
              yield
                <.li(
                  ^.key := s"li-$i",
                  ^.id := s"teli-$i",
                  ^.style := js.Dictionary("marginLeft" -> s"${task.indentation * 30}px"),
                  VdomAttr("num") := i,
                  task.content.toVdomNode
                )).toVdomArray
          )
        ),
        <.br(),
        "----------------",
        <.br(),
        <.br(),
        <.br(),
        (for ((task, i) <- state.document.tasks.zipWithIndex)
          yield <.div(^.key := s"task-$i", "- [", task.indentation, "]", task.content.toVdomNode)).toVdomArray
      )
    }

    private def handleEvent(name: String)(event: ReactEventFromInput): Callback = LogExceptionsCallback {
      val selection = IndexedCursor.tupleFromSelection(dom.window.getSelection())
      console.log(s"$name EVENT", event.nativeEvent, event.eventType, selection.toString)
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
      val document = $.state.runNow().document

      if (selection.start != selection.end) {
        val ClipboardData(htmlText, plainText) = convertToClipboardData(document, selection)

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

    private def handleKeyDown(event: SyntheticKeyboardEvent[_]): Callback = logExceptions {
      val selection = IndexedCursor.tupleFromSelection(dom.window.getSelection())
      val IndexedSelection(start, end) = selection
      implicit val document = $.state.runNow().document
      val shiftPressed = event.shiftKey
      val altPressed = event.altKey
      val ctrlPressed = event.ctrlKey // TODO: Set to metaKey when Mac OS X
      val formatting = document.tasks(start.seqIndex).content.formattingAtCursor(start.offsetInTask)

      event.key match {
        case eventKey if eventKey.length == 1 && !ctrlPressed && !(altPressed && shiftPressed) =>
          event.preventDefault()
          replaceSelectionInState(
            replacement = Replacement.fromString(eventKey, formatting),
            IndexedSelection(start, end))

        case "Enter" if !ctrlPressed =>
          event.preventDefault()
          if (shiftPressed) {
            replaceSelectionInState(
              replacement = Replacement.fromString("\n", formatting),
              IndexedSelection(start, end))
          } else {
            replaceSelectionInState(replacement = Replacement.newEmptyTask(), IndexedSelection(start, end))
          }

        case "Enter" if ctrlPressed =>
          // Open link
          getAnyLinkInSelection(selection) match {
            case Some(link) => dom.window.open(link, "_blank")
            case None       =>
          }
          Callback.empty

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

        case "i" if ctrlPressed =>
          event.preventDefault()
          toggleFormatting((form, value) => form.copy(italic = value), selection)
        case "b" if ctrlPressed =>
          event.preventDefault()
          toggleFormatting((form, value) => form.copy(bold = value), selection)
        case "C" if shiftPressed && altPressed =>
          event.preventDefault()
          toggleFormatting((form, value) => form.copy(code = value), selection)
        case "\\" if ctrlPressed =>
          event.preventDefault()
          toggleFormatting((form, value) => Formatting.none, selection)

        case "u" if ctrlPressed =>
          // Disable underline modifier
          event.preventDefault()
          Callback.empty

        case "z" if ctrlPressed && !shiftPressed =>
          event.preventDefault()
          applyHistoryEdit(editHistory.undo())

        case "Z" if ctrlPressed && shiftPressed =>
          event.preventDefault()
          applyHistoryEdit(editHistory.redo())

        case "y" if ctrlPressed =>
          event.preventDefault()
          applyHistoryEdit(editHistory.redo())

        case "k" if ctrlPressed =>
          // Edit link
          event.preventDefault()
          editLink(selection)

        case _ =>
          Callback.empty
      }
    }

    private def applyHistoryEdit(maybeEdit: Option[EditHistory.Edit]): Callback = maybeEdit match {
      case None => Callback.empty
      case Some(edit) =>
        val documentStore = $.props.runNow().documentStore
        val oldDocument = $.state.runNow().document
        val newDocument = documentStore
          .replaceTasksWithoutCallingListeners(toReplace = edit.removedTasks, toAdd = edit.addedTasks)
        $.modState(
          _.copy(document = newDocument),
          setSelection(edit.selectionAfterEdit.attachToDocument(newDocument))
        )
    }

    private def replaceSelectionInState(replacement: Replacement,
                                        selectionBeforeEdit: IndexedSelection): Callback = {
      val IndexedSelection(start, end) = selectionBeforeEdit
      val oldDocument = $.state.runNow().document

      def tasksOption(document: Document, index: Int): Option[Task] = index match {
        case i if i < 0                      => None
        case i if i >= document.tasks.length => None
        case _                               => Some(document.tasks(index))
      }

      val newOrderTokens = {
        val previousTask = tasksOption(oldDocument, start.seqIndex - 1)
        val nextTask = tasksOption(oldDocument, end.seqIndex + 1)
        OrderToken.evenlyDistributedValuesBetween(
          numValues = replacement.parts.length,
          lower = previousTask.map(_.orderToken),
          higher = nextTask.map(_.orderToken)
        )
      }
      val baseIndentation = oldDocument.tasks(start.seqIndex).indentation
      val tasksToReplace = for (i <- start.seqIndex to end.seqIndex) yield oldDocument.tasks(i)
      val tasksToAdd =
        for (((replacementPart, newOrderToken), i) <- (replacement.parts zip newOrderTokens).zipWithIndex)
          yield {
            def ifIndexOrEmpty(index: Int)(tags: TextWithMarkup): TextWithMarkup =
              if (i == index) tags else TextWithMarkup.empty
            Task.withRandomId(
              content = ifIndexOrEmpty(0)(
                oldDocument.tasks(start.seqIndex).content.sub(0, start.offsetInTask)) +
                replacementPart.content +
                ifIndexOrEmpty(replacement.parts.length - 1)(
                  oldDocument.tasks(end.seqIndex).content.sub(end.offsetInTask)),
              orderToken = newOrderToken,
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
      val oldDocument = $.state.runNow().document

      val IndexedSelection(start, end) = selection
      val tasksToReplace = for (i <- start.seqIndex to end.seqIndex) yield oldDocument.tasks(i)
      val tasksToAdd = tasksToReplace.map(
        task =>
          Task.withRandomId(
            content = task.content,
            orderToken = task.orderToken,
            indentation = zeroIfNegative(task.indentation + indentIncrease)))

      replaceInStateWithHistory(
        tasksToReplace = tasksToReplace,
        tasksToAdd = tasksToAdd,
        selectionBeforeEdit = selection,
        selectionAfterEdit = selection
      )
    }

    private def toggleFormatting(updateFunc: (Formatting, Boolean) => Formatting,
                                 selection: IndexedSelection)(implicit document: Document): Callback = {
      val IndexedSelection(start, end) = selection

      def toggleFormattingInternal(start: IndexedCursor, end: IndexedCursor): Callback = {
        def setFormatting(tasks: Seq[Task], value: Boolean): Seq[Task] =
          for (task <- tasks)
            yield
              Task.withRandomId(
                content = task.content
                  .withFormatting(
                    beginOffset = if (task == tasks.head) start.offsetInTask else 0,
                    endOffset = if (task == tasks.last) end.offsetInTask else task.contentString.length,
                    formatting => updateFunc(formatting, value)
                  ),
                orderToken = task.orderToken,
                indentation = task.indentation
              )

        val oldDocument = $.state.runNow().document
        val tasksToReplace = for (i <- start.seqIndex to end.seqIndex) yield oldDocument.tasks(i)
        val tasksToAdd = {
          val candidate = setFormatting(tasksToReplace, value = true)
          if (candidate.map(_.content) == tasksToReplace.map(_.content)) {
            setFormatting(tasksToReplace, value = false)
          } else {
            candidate
          }
        }

        replaceInStateWithHistory(
          tasksToReplace = tasksToReplace,
          tasksToAdd = tasksToAdd,
          selectionBeforeEdit = selection,
          selectionAfterEdit = selection
        )
      }

      if (start == end) {
        // update whole line
        toggleFormattingInternal(start.toStartOfTask, end.toEndOfTask)
      } else {
        toggleFormattingInternal(start, end)
      }
    }

    private def editLink(originalSelection: IndexedSelection): Callback = {
      implicit val oldDocument = $.state.runNow().document

      def expandSelection(selection: IndexedSelection): IndexedSelection = {
        def expand(link: String, cursor: IndexedCursor, direction: Int): IndexedCursor = {
          val content = oldDocument.tasks(cursor.seqIndex).content
          cursor.offsetInTask match {
            case 0 if direction < 0                                                => cursor
            case offset if offset == content.contentString.length && direction > 0 => cursor
            case offset =>
              val nextChar =
                if (direction == -1) content.sub(offset - 1, offset) else content.sub(offset, offset + 1)
              if (nextChar.anyLink == Some(link)) {
                expand(link, cursor plusOffset direction, direction)
              } else {
                cursor
              }
          }
        }

        val IndexedSelection(start, end) = selection
        getAnyLinkInSelection(selection) match {
          case Some(link) if selection.isCollapsed =>
            IndexedSelection(expand(link, start, -1), expand(link, end, 1))
          case _ => selection
        }
      }

      class CancelException extends Exception
      def newLinkFromDialog(defaultValue: Option[String]): Option[String] = {
        val result = dom.window.prompt("Edit link", defaultValue getOrElse "")
        result match {
          case null => throw new CancelException
          case ""   => None
          case s    => Some(s)
        }
      }

      def editLinkInternal(selection: IndexedSelection, newLink: Option[String]): Callback = {
        val IndexedSelection(start, end) = selection

        val tasksToReplace = for (i <- start.seqIndex to end.seqIndex) yield oldDocument.tasks(i)
        val tasksToAdd = {
          for (task <- tasksToReplace)
            yield
              Task.withRandomId(
                content = task.content
                  .withFormatting(
                    beginOffset = if (task == tasksToReplace.head) start.offsetInTask else 0,
                    endOffset =
                      if (task == tasksToReplace.last) end.offsetInTask else task.contentString.length,
                    updateFunc = _.copy(link = newLink)
                  ),
                orderToken = task.orderToken,
                indentation = task.indentation
              )
        }

        replaceInStateWithHistory(
          tasksToReplace = tasksToReplace,
          tasksToAdd = tasksToAdd,
          selectionBeforeEdit = originalSelection,
          selectionAfterEdit = selection
        )
      }

      expandSelection(originalSelection) match {
        case s if s.isCollapsed => Callback.empty
        case expandedSelection =>
          try {
            val newLink = newLinkFromDialog(getAnyLinkInSelection(originalSelection))
            editLinkInternal(expandedSelection, newLink)
          } catch {
            case _: CancelException => Callback.empty
          }
      }
    }

    private def replaceInStateWithHistory(tasksToReplace: Seq[Task],
                                          tasksToAdd: Seq[Task],
                                          selectionBeforeEdit: IndexedSelection,
                                          selectionAfterEdit: IndexedSelection,
                                          replacementString: String = ""): Callback = {
      def isNoOp: Boolean = {
        if (tasksToReplace.size == tasksToAdd.size && selectionBeforeEdit == selectionAfterEdit) {
          if (tasksToReplace.isEmpty) {
            true
          } else {
            (tasksToReplace.sorted zip tasksToAdd.sorted).forall {
              case (t1, t2) => t1 equalsIgnoringId t2
            }
          }
        } else {
          false
        }
      }

      if (isNoOp) {
        Callback.empty
      } else {
        val documentStore = $.props.runNow().documentStore
        val oldDocument = $.state.runNow().document
        val newDocument =
          documentStore.replaceTasksWithoutCallingListeners(toReplace = tasksToReplace, toAdd = tasksToAdd)

        $.modState(
          _.copy(document = newDocument), {
            editHistory.addEdit(
              removedTasks = tasksToReplace,
              addedTasks = tasksToAdd,
              selectionBeforeEdit = selectionBeforeEdit.detach(oldDocument),
              selectionAfterEdit = selectionAfterEdit.detach(newDocument),
              replacementString = replacementString
            )
            setSelection(selectionAfterEdit)
          }
        )
      }
    }
  }

  // **************** Helper classes and methods **************** //
  @visibleForTesting private[desktop] case class Replacement(parts: Seq[Replacement.Part]) {
    def contentString: String = parts.map(_.contentString).mkString
  }
  @visibleForTesting private[desktop] object Replacement {
    def create(firstPartContent: TextWithMarkup, otherParts: Part*): Replacement =
      Replacement(Part(firstPartContent, indentationRelativeToCurrent = 0) :: List(otherParts: _*))
    def fromString(string: String, formatting: Formatting): Replacement =
      Replacement.create(TextWithMarkup(string, formatting))
    def newEmptyTask(indentationRelativeToCurrent: Int = 0): Replacement =
      Replacement.create(
        TextWithMarkup.empty,
        Part(content = TextWithMarkup.empty, indentationRelativeToCurrent = indentationRelativeToCurrent))
    def empty: Replacement = Replacement.create(TextWithMarkup.empty)

    case class Part(content: TextWithMarkup, indentationRelativeToCurrent: Int) {
      def contentString: String = content.contentString
    }
  }
  @visibleForTesting private[desktop] case class ClipboardData(htmlText: String, plainText: String)
  @visibleForTesting private[desktop] def convertToClipboardData(
      document: Document,
      selection: IndexedSelection): ClipboardData = {
    case class Subtask(task: Task, startOffset: Int, endOffset: Int) {
      def content: TextWithMarkup = task.content.sub(startOffset, endOffset)
      def indentation: Int = task.indentation
    }

    val IndexedSelection(start, end) = selection
    val subtasks = document.tasks.zipWithIndex
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
          resultBuilder.append(subtask.content.toHtml)
          resultBuilder.append("</li>")
          lastIndentation = subtask.indentation
        }
        for (i <- baseIndentation until lastIndentation) {
          resultBuilder.append("</ul>")
        }
        resultBuilder.toString
      },
      plainText = subtasks.map(_.content.contentString).mkString("\n")
    )
  }

  @visibleForTesting private[desktop] def clipboardStringToReplacement(
      clipboardString: String): Replacement = {
    val html = {
      val resultHolder = dom.document.createElement("span")
      resultHolder.innerHTML = clipboardString
      resultHolder
    }

    def containsListItem(node: dom.raw.Node): Boolean = {
      if (nodeIsLi(node)) {
        true
      } else {
        children(node).exists(containsListItem)
      }
    }

    val partsBuilder = mutable.Buffer[Replacement.Part]()

    def addPastedText(nodes: Seq[dom.raw.Node], nextRelativeIndentation: Int): Unit = {
      val childNodesWithoutLi = mutable.Buffer[dom.raw.Node]()
      def pushChildNodesWithoutLi(): Unit = {
        if (childNodesWithoutLi.nonEmpty) {
          val parsedText = TextWithMarkup.fromHtmlNodes(childNodesWithoutLi: _*)
          for (line <- Splitter.on('\n').omitEmptyStrings().trimResults().split(parsedText.contentString)) {
            partsBuilder.append(
              Replacement.Part(TextWithMarkup(line), zeroIfNegative(nextRelativeIndentation)))
          }
          childNodesWithoutLi.clear()
        }
      }

      for (node <- nodes) {
        if (containsListItem(node)) {
          pushChildNodesWithoutLi()
          if (nodeIsLi(node)) {
            partsBuilder.append(
              Replacement.Part(TextWithMarkup.fromHtmlNodes(node), zeroIfNegative(nextRelativeIndentation)))
          } else {
            addPastedText(
              children(node),
              nextRelativeIndentation =
                if (nodeIsList(node)) nextRelativeIndentation + 1 else nextRelativeIndentation)

          }
        } else {
          childNodesWithoutLi.append(node)
        }
      }

      pushChildNodesWithoutLi()
    }

    addPastedText(Seq(html), nextRelativeIndentation = -1)
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

  private def getAnyLinkInSelection(selection: IndexedSelection)(
      implicit document: Document): Option[String] = {
    if (selection.isCollapsed) {
      val cursor = selection.start
      val expandedSelection = IndexedSelection(
        if (cursor.offsetInTask == 0) cursor else cursor minusOffset 1,
        if (cursor == cursor.toEndOfTask) cursor else cursor plusOffset 1)
      if (expandedSelection.isCollapsed) {
        None
      } else {
        getAnyLinkInSelection(expandedSelection)
      }
    } else {
      val IndexedSelection(start, end) = selection
      val texts = for (i <- start.seqIndex to end.seqIndex) yield {
        document
          .tasks(i)
          .content
          .sub(
            beginOffset = if (i == start.seqIndex) start.offsetInTask else 0,
            endOffset = if (i == end.seqIndex) end.offsetInTask else -1)
      }
      texts.flatMap(_.anyLink).headOption
    }
  }

  private def zeroIfNegative(i: Int): Int = if (i < 0) 0 else i
}
