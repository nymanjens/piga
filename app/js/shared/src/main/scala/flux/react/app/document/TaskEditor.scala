package flux.react.app.document

import common.DomNodeUtils._
import common.GuavaReplacement.Splitter
import common.LoggingUtils.{LogExceptionsCallback, logExceptions}
import common.ScalaUtils.{ifThenOption, visibleForTesting}
import common.time.Clock
import common.{DomNodeUtils, I18n, OrderToken}
import flux.react.ReactVdomUtils.^^
import flux.react.app.document.KeyCombination._
import flux.react.router.RouterContext
import flux.stores.StateStore
import flux.stores.document.DocumentStore
import japgolly.scalajs.react._
import japgolly.scalajs.react.raw.SyntheticKeyboardEvent
import japgolly.scalajs.react.vdom.PackageBase.VdomAttr
import japgolly.scalajs.react.vdom.html_<^._
import models.access.EntityAccess
import models.document.Document.{DetachedCursor, IndexedCursor, IndexedSelection}
import models.document.TextWithMarkup.Formatting
import models.document.{Document, Task, TextWithMarkup}
import org.scalajs.dom
import org.scalajs.dom.console

import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.scalajs.js

private[document] final class TaskEditor(implicit entityAccess: EntityAccess, i18n: I18n, clock: Clock) {

  private val component = ScalaComponent
    .builder[Props](getClass.getSimpleName)
    .initialStateFromProps(props => State(document = props.documentStore.state.document))
    .renderBackend[Backend]
    .componentWillMount(scope => scope.backend.willMount(scope.props, scope.state))
    .componentWillUnmount(scope => scope.backend.willUnmount(scope.props))
    .build

  // **************** API ****************//
  def apply(documentStore: DocumentStore)(implicit router: RouterContext): VdomElement = {
    component(Props(documentStore, router))
  }

  // **************** Private inner types ****************//
  private case class Props(documentStore: DocumentStore, router: RouterContext)
  private case class State(document: Document)

  private class Backend($ : BackendScope[Props, State]) extends StateStore.Listener {

    private val editHistory: EditHistory = new EditHistory()
    private var lastSingletonFormating: SingletonFormating = SingletonFormating(
      cursor = DetachedCursor(
        task = Task.withRandomId(
          TextWithMarkup.empty,
          OrderToken.middle,
          indentation = 0,
          collapsed = false,
          delayedUntil = None,
          tags = Seq()),
        offsetInTask = 0),
      formatting = Formatting.none
    )

    def willMount(props: Props, state: State): Callback = LogExceptionsCallback {
      props.documentStore.register(this)
      $.modState(state => logExceptions(state.copy(document = props.documentStore.state.document))).runNow()
    }

    def willUnmount(props: Props): Callback = LogExceptionsCallback {
      props.documentStore.deregister(this)
    }

    override def onStateUpdate() = {
      val props = $.props.runNow()
      $.modState(state => logExceptions(state.copy(document = props.documentStore.state.document))).runNow()
    }

    def render(props: Props, state: State): VdomElement = logExceptions {
      implicit val router = props.router
      <.span(
        <.div(
          ^.contentEditable := true,
          ^.className := "task-editor",
          ^.spellCheck := false,
          VdomAttr("suppressContentEditableWarning") := true,
          ^.onKeyDown ==> handleKeyDown,
          ^.onPaste ==> handlePaste,
          ^.onCut ==> handleCut,
          ^.onCopy ==> handleCopy,
          ^.onInput ==> handleEvent("onChange"),
          ^.onBeforeInput ==> handleEvent("onBeforeInput"),
          <.ul(
            applyCollapsedProperty(state.document.tasks).map {
              case (task, i, maybeAmountCollapsed) =>
                val nodeType = state.document.tasksOption(i + 1) match {
                  case _ if task.indentation == 0                                => "root"
                  case Some(nextTask) if nextTask.indentation > task.indentation => "node"
                  case _                                                         => "leaf"
                }
                (<.li(
                  ^.key := s"li-$i",
                  ^.id := s"teli-$i",
                  ^.style := js.Dictionary("marginLeft" -> s"${task.indentation * 30}px"),
                  ^^.classes(Seq(nodeType) ++ ifThenOption(task.collapsed)("collapsed")),
                  VdomAttr("num") := i,
                  task.content.toVdomNode
                ) +: (maybeAmountCollapsed match {
                  case Some(amountCollapsed) =>
                    Seq(
                      <.styleTag(
                        ^.key := s"listyle-$i",
                        s"""#teli-$i:after {content: "  {+ $amountCollapsed}";}"""))
                  case None => Seq()
                })).toVdomArray
            }.toVdomArray
          )
        )
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
      replaceSelection(replacement = Replacement.empty, selection)
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
      val IndexedSelection(start, end) = selection
      implicit val document = $.state.runNow().document
      val formatting =
        if (lastSingletonFormating.cursor == start.detach) {
          lastSingletonFormating.formatting
        } else {
          document.tasks(start.seqIndex).content.formattingAtCursor(start.offsetInTask)
        }

      replaceSelection(
        replacement = clipboardStringToReplacement(getAnyClipboardString(event), baseFormatting = formatting),
        selection)
    }

    private def handleKeyDown(event: SyntheticKeyboardEvent[_]): Callback = logExceptions {
      val selection = IndexedCursor.tupleFromSelection(dom.window.getSelection())
      val IndexedSelection(start, end) = selection
      implicit val document = $.state.runNow().document
      val formatting =
        if (lastSingletonFormating.cursor == start.detach) {
          lastSingletonFormating.formatting
        } else {
          document.tasks(start.seqIndex).content.formattingAtCursor(start.offsetInTask)
        }

      // console.log(s"event.key = ${event.key}")

      val keyCombination = KeyCombination.fromEvent(event)
      keyCombination match {
        case CharacterKey(character, /* ctrlOrMeta */ false, shift, alt) if !(shift && alt) =>
          event.preventDefault()
          replaceSelection(
            replacement = Replacement.fromString(character.toString, formatting),
            IndexedSelection(start, end))

        case SpecialKey(Enter, /* ctrlOrMeta */ false, /* shift */ _, /* alt */ false) =>
          event.preventDefault()
          if (keyCombination.shift) {
            replaceSelection(
              replacement = Replacement.fromString("\n", formatting),
              IndexedSelection(start, end))
          } else {
            replaceSelection(replacement = Replacement.newEmptyTask(), IndexedSelection(start, end))
          }

        case SpecialKey(Backspace, /* ctrlOrMeta */ _, /* shift */ false, /* alt */ false) =>
          event.preventDefault()
          if (start == end) {
            if (keyCombination.ctrlOrMeta) {
              replaceSelection(replacement = Replacement.empty, IndexedSelection(start.minusWord, end))
            } else {
              replaceSelection(
                replacement = Replacement.empty,
                IndexedSelection(start minusOffsetInSeq 1, end))
            }
          } else {
            replaceSelection(replacement = Replacement.empty, IndexedSelection(start, end))
          }

        case SpecialKey(Delete, /* ctrlOrMeta */ _, /* shift */ false, /* alt */ false) =>
          event.preventDefault()
          if (start == end) {
            if (keyCombination.ctrlOrMeta) {
              replaceSelection(replacement = Replacement.empty, IndexedSelection(start, end.plusWord))
            } else {
              replaceSelection(
                replacement = Replacement.empty,
                IndexedSelection(start, end plusOffsetInSeq 1))
            }
          } else {
            replaceSelection(replacement = Replacement.empty, IndexedSelection(start, end))
          }

        case SpecialKey(Tab, /* ctrlOrMeta */ false, /* shift */ _, /* alt */ false) =>
          event.preventDefault()
          val indentIncrease = if (keyCombination.shift) -1 else 1
          updateTasksInSelection(selection, updateCollapsedChildren = true) { task =>
            task.copyWithRandomId(indentation = zeroIfNegative(task.indentation + indentIncrease))
          }

        // Italic
        case CharacterKey('i', /* ctrlOrMeta */ true, /* shift */ false, /* alt */ false) =>
          event.preventDefault()
          toggleFormatting(
            (form, value) => form.copy(italic = value),
            selection,
            formattingAtStart = formatting)

        // Bold
        case CharacterKey('b', /* ctrlOrMeta */ true, /* shift */ false, /* alt */ false) =>
          event.preventDefault()
          toggleFormatting(
            (form, value) => form.copy(bold = value),
            selection,
            formattingAtStart = formatting)

        // Code font
        case CharacterKey('C', /* ctrlOrMeta */ false, /* shift */ true, /* alt */ true) =>
          event.preventDefault()
          toggleFormatting(
            (form, value) => form.copy(code = value),
            selection,
            formattingAtStart = formatting)

        // Strikethrough (Alt + Shift + 5)
        case CharacterKey(_, /* ctrlOrMeta */ false, /* shift */ true, /* alt */ true)
            if event.keyCode == 53 =>
          event.preventDefault()
          toggleFormatting(
            (form, value) => form.copy(strikethrough = value),
            selection,
            formattingAtStart = formatting)

        // Clear formatting
        case CharacterKey('\\', /* ctrlOrMeta */ true, /* shift */ false, /* alt */ false) =>
          event.preventDefault()
          toggleFormatting((form, value) => Formatting.none, selection, formattingAtStart = formatting)

        // Disable underline modifier
        case CharacterKey('u', /* ctrlOrMeta */ true, /* shift */ false, /* alt */ false) =>
          event.preventDefault()
          Callback.empty

        // Disable save shortcut
        case CharacterKey('s', /* ctrlOrMeta */ true, /* shift */ false, /* alt */ false) =>
          event.preventDefault()
          Callback.empty

        // Undo
        case CharacterKey('z', /* ctrlOrMeta */ true, /* shift */ false, /* alt */ false) =>
          event.preventDefault()
          applyHistoryEdit(editHistory.undo())

        // Redo
        case CharacterKey('Z', /* ctrlOrMeta */ true, /* shift */ true, /* alt */ false) =>
          event.preventDefault()
          applyHistoryEdit(editHistory.redo())

        // Redo
        case CharacterKey('y', /* ctrlOrMeta */ true, /* shift */ false, /* alt */ false) =>
          event.preventDefault()
          applyHistoryEdit(editHistory.redo())

        // Edit link
        case CharacterKey('k', /* ctrlOrMeta */ true, /* shift */ false, /* alt */ false) =>
          event.preventDefault()
          editLink(selection)

        // Open link
        case SpecialKey(Enter, /* ctrlOrMeta */ true, /* shift */ false, /* alt */ false) =>
          getAnyLinkInSelection(selection) match {
            case Some(link) => dom.window.open(link, "_blank")
            case None       =>
          }
          Callback.empty

        // Select word
        case CharacterKey('m', /* ctrlOrMeta */ true, /* shift */ false, /* alt */ false) =>
          event.preventDefault()
          selectExtendedWordAround(start)

        // Select line
        case CharacterKey('j', /* ctrlOrMeta */ true, /* shift */ false, /* alt */ false) =>
          event.preventDefault()
          setSelection(IndexedSelection(start.toStartOfTask, start.toEndOfTask))

        // Delete line
        case CharacterKey('d', /* ctrlOrMeta */ true, /* shift */ false, /* alt */ false) =>
          event.preventDefault()
          removeTasks(selection.seqIndices)

        // Duplicate line
        case CharacterKey('B', /* ctrlOrMeta */ true, /* shift */ true, /* alt */ false) =>
          event.preventDefault()
          duplicateTasks(selection.seqIndices, selectionBeforeEdit = selection)

        // Move lines up
        case SpecialKey(ArrowUp, /* ctrlOrMeta */ false, /* shift */ false, /* alt */ true) =>
          event.preventDefault()
          moveLinesInSeq(selection, direction = -1)

        // Move lines down
        case SpecialKey(ArrowDown, /* ctrlOrMeta */ false, /* shift */ false, /* alt */ true) =>
          event.preventDefault()
          moveLinesInSeq(selection, direction = +1)

        // Expand lines
        case CharacterKey('=', /* ctrlOrMeta */ true, /* shift */ false, /* alt */ false) =>
          event.preventDefault()
          updateTasksInSelection(selection, updateCollapsedChildren = false) { task =>
            task.copyWithRandomId(collapsed = false)
          }

        // Collapse lines
        case CharacterKey('-', /* ctrlOrMeta */ true, /* shift */ false, /* alt */ false) =>
          event.preventDefault()
          updateTasksInSelection(selection, updateCollapsedChildren = false) { task =>
            task.copyWithRandomId(collapsed = true)
          }

        // Convert to upper case
        case CharacterKey('U', /* ctrlOrMeta */ true, /* shift */ true, /* alt */ false) =>
          event.preventDefault()
          updateCharactersInSelection(selection, _.toUpperCase)

        // Convert to lower case
        case CharacterKey('L', /* ctrlOrMeta */ true, /* shift */ true, /* alt */ false) =>
          event.preventDefault()
          updateCharactersInSelection(selection, _.toLowerCase)

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

    private def replaceSelection(replacement: Replacement,
                                 selectionBeforeEdit: IndexedSelection): Callback = {
      val IndexedSelection(start, end) = selectionBeforeEdit
      val oldDocument = $.state.runNow().document

      val newOrderTokens = {
        val previousTask = oldDocument.tasksOption(start.seqIndex - 1)
        val nextTask = oldDocument.tasksOption(end.seqIndex + 1)
        OrderToken.evenlyDistributedValuesBetween(
          numValues = replacement.parts.length,
          lower = previousTask.map(_.orderToken),
          higher = nextTask.map(_.orderToken)
        )
      }
      val tasksToReplace = for (i <- selectionBeforeEdit.seqIndices) yield oldDocument.tasks(i)
      val tasksToAdd =
        for (((replacementPart, newOrderToken), i) <- (replacement.parts zip newOrderTokens).zipWithIndex)
          yield {
            def ifIndexOrEmpty(index: Int)(tags: TextWithMarkup): TextWithMarkup =
              if (i == index) tags else TextWithMarkup.empty
            tasksToReplace.head.copyWithRandomId(
              content = ifIndexOrEmpty(0)(
                oldDocument.tasks(start.seqIndex).content.sub(0, start.offsetInTask)) +
                replacementPart.content +
                ifIndexOrEmpty(replacement.parts.length - 1)(
                  oldDocument.tasks(end.seqIndex).content.sub(end.offsetInTask)),
              orderToken = newOrderToken,
              indentation = tasksToReplace.head.indentation + replacementPart.indentationRelativeToCurrent
            )
          }

      replaceWithHistory(
        tasksToReplace = tasksToReplace,
        tasksToAdd = tasksToAdd,
        selectionBeforeEdit = selectionBeforeEdit,
        selectionAfterEdit = IndexedSelection.singleton(
          (start proceedNTasks (replacement.parts.length - 1)) plusOffset replacement.parts.last.contentString.length),
        replacementString = replacement.contentString
      )
    }

    private def removeTasks(taskIndices: Range): Callback = {
      implicit val oldDocument = $.state.runNow().document

      val tasksToReplace = for (i <- taskIndices) yield oldDocument.tasks(i)
      val tasksToAdd =
        if (oldDocument.tasks.size > taskIndices.size) Seq()
        else // Removing all tasks in this document --> Replace the last task with an empty task
          Seq(
            Task.withRandomId(
              content = TextWithMarkup.empty,
              orderToken = OrderToken.middle,
              indentation = 0,
              collapsed = false,
              delayedUntil = None,
              tags = Seq()
            ))

      replaceWithHistory(
        tasksToReplace = tasksToReplace,
        tasksToAdd = tasksToAdd,
        selectionBeforeEdit = IndexedSelection(
          IndexedCursor.atStartOfLine(taskIndices.head),
          IndexedCursor.atEndOfLine(taskIndices.last)),
        selectionAfterEdit = IndexedSelection.singleton(
          IndexedCursor.atStartOfLine(
            if (oldDocument.tasks.size > taskIndices.head + taskIndices.size) taskIndices.head
            else if (taskIndices.head == 0) 0
            else taskIndices.head - 1))
      )
    }

    private def duplicateTasks(taskIndices: Range, selectionBeforeEdit: IndexedSelection): Callback = {
      implicit val oldDocument = $.state.runNow().document

      val newOrderTokens = {
        val taskBefore = oldDocument.tasksOption(taskIndices.last)
        val taskAfter = oldDocument.tasksOption(taskIndices.last + 1)
        OrderToken.evenlyDistributedValuesBetween(
          numValues = taskIndices.size,
          lower = taskBefore.map(_.orderToken),
          higher = taskAfter.map(_.orderToken)
        )
      }
      val tasksToAdd = for ((i, orderToken) <- taskIndices zip newOrderTokens)
        yield oldDocument.tasks(i).copyWithRandomId(orderToken = orderToken)

      replaceWithHistory(
        tasksToReplace = Seq(),
        tasksToAdd = tasksToAdd,
        selectionBeforeEdit = selectionBeforeEdit,
        selectionAfterEdit = IndexedSelection(
          selectionBeforeEdit.start.plusLines(taskIndices.size),
          selectionBeforeEdit.end.plusLines(taskIndices.size))
      )
    }

    private def updateTasksInSelection(selection: IndexedSelection, updateCollapsedChildren: Boolean)(
        taskUpdate: Task => Task): Callback = {
      implicit val oldDocument = $.state.runNow().document

      val IndexedSelection(start, end) =
        if (updateCollapsedChildren) selection.includeCollapsedChildren else selection
      val tasksToReplace = for (i <- start.seqIndex to end.seqIndex) yield oldDocument.tasks(i)
      val tasksToAdd = tasksToReplace.map(taskUpdate)

      replaceWithHistory(
        tasksToReplace = tasksToReplace,
        tasksToAdd = tasksToAdd,
        selectionBeforeEdit = selection,
        selectionAfterEdit = selection
      )
    }

    private def updateCharactersInSelection(selection: Document.IndexedSelection,
                                            characterTransform: String => String): Callback = {
      implicit val oldDocument = $.state.runNow().document
      val tasksToReplace = for (i <- selection.seqIndices) yield oldDocument.tasks(i)
      val tasksToAdd =
        for (task <- tasksToReplace)
          yield
            task.copyWithRandomId(
              content = task.content.withTransformedCharacters(
                beginOffset = selection.startOffsetInTask(task),
                endOffset = selection.endOffsetInTask(task),
                characterTransform = characterTransform
              )
            )

      replaceWithHistory(
        tasksToReplace = tasksToReplace,
        tasksToAdd = tasksToAdd,
        selectionBeforeEdit = selection,
        selectionAfterEdit = selection
      )
    }

    private def toggleFormatting(updateFunc: (Formatting, Boolean) => Formatting,
                                 selection: IndexedSelection,
                                 formattingAtStart: Formatting)(implicit document: Document): Callback = {
      val IndexedSelection(start, end) = selection

      def toggleFormattingInternal(start: IndexedCursor, end: IndexedCursor): Callback = {
        def setFormatting(tasks: Seq[Task], value: Boolean): Seq[Task] =
          for (task <- tasks)
            yield
              task.copyWithRandomId(
                content = task.content.withFormatting(
                  beginOffset = if (task == tasks.head) start.offsetInTask else 0,
                  endOffset = if (task == tasks.last) end.offsetInTask else task.contentString.length,
                  formatting => updateFunc(formatting, value)
                ))

        val oldDocument = $.state.runNow().document
        val tasksToReplace = for (i <- selection.seqIndices) yield oldDocument.tasks(i)
        val tasksToAdd = {
          val candidate = setFormatting(tasksToReplace, value = true)
          if (candidate.map(_.content) == tasksToReplace.map(_.content)) {
            setFormatting(tasksToReplace, value = false)
          } else {
            candidate
          }
        }

        replaceWithHistory(
          tasksToReplace = tasksToReplace,
          tasksToAdd = tasksToAdd,
          selectionBeforeEdit = selection,
          selectionAfterEdit = selection
        )
      }

      if (start == end) {
        lastSingletonFormating = SingletonFormating(
          start.detach,
          formatting =
            if (updateFunc(formattingAtStart, true) == formattingAtStart) updateFunc(formattingAtStart, false)
            else updateFunc(formattingAtStart, true)
        )
        Callback.empty
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
          case Some(link) if selection.isSingleton =>
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
        val tasksToReplace = for (i <- selection.seqIndices) yield oldDocument.tasks(i)
        val tasksToAdd = {
          for (task <- tasksToReplace)
            yield
              task.copyWithRandomId(
                content = task.content
                  .withFormatting(
                    beginOffset = selection.startOffsetInTask(task),
                    endOffset = selection.endOffsetInTask(task),
                    updateFunc = _.copy(link = newLink)
                  ))
        }

        replaceWithHistory(
          tasksToReplace = tasksToReplace,
          tasksToAdd = tasksToAdd,
          selectionBeforeEdit = originalSelection,
          selectionAfterEdit = selection
        )
      }

      expandSelection(originalSelection) match {
        case s if s.isSingleton => Callback.empty
        case expandedSelection =>
          try {
            val newLink = newLinkFromDialog(getAnyLinkInSelection(originalSelection))
            editLinkInternal(expandedSelection, newLink)
          } catch {
            case _: CancelException => Callback.empty
          }
      }
    }

    private def moveLinesInSeq(selectionBeforeEdit: IndexedSelection, direction: Int): Callback = {
      implicit val oldDocument = $.state.runNow().document
      val IndexedSelection(start, end) = selectionBeforeEdit.includeCollapsedChildren

      val seqIndexMovement = {
        val indexThatIsHoppedOver = if (direction < 0) start.seqIndex - 1 else end.seqIndex + 1
        oldDocument.collapsedTasksRange(indexThatIsHoppedOver) match {
          case None                    => direction * 1
          case Some(adjacentTaskRange) => direction * adjacentTaskRange.numberOfTasks
        }
      }

      val (task1, task2) =
        if (seqIndexMovement < 0)
          (
            oldDocument.tasksOption(start.seqIndex + seqIndexMovement - 1),
            oldDocument.tasksOption(start.seqIndex + seqIndexMovement))
        else
          (
            oldDocument.tasksOption(end.seqIndex + seqIndexMovement),
            oldDocument.tasksOption(end.seqIndex + seqIndexMovement + 1))

      if (task1.isEmpty && task2.isEmpty) {
        Callback.empty
      } else {

        val tasksToReplace = for (i <- start.seqIndex to end.seqIndex) yield oldDocument.tasks(i)

        val newOrderTokens = {
          OrderToken.evenlyDistributedValuesBetween(
            numValues = tasksToReplace.length,
            lower = task1.map(_.orderToken),
            higher = task2.map(_.orderToken)
          )
        }
        val tasksToAdd =
          for ((task, newOrderToken) <- tasksToReplace zip newOrderTokens)
            yield task.copyWithRandomId(orderToken = newOrderToken)

        replaceWithHistory(
          tasksToReplace = tasksToReplace,
          tasksToAdd = tasksToAdd,
          selectionBeforeEdit = selectionBeforeEdit,
          selectionAfterEdit = IndexedSelection(
            selectionBeforeEdit.start.copy(seqIndex = selectionBeforeEdit.start.seqIndex + seqIndexMovement),
            selectionBeforeEdit.end.copy(seqIndex = selectionBeforeEdit.end.seqIndex + seqIndexMovement)
          )
        )
      }
    }

    private def selectExtendedWordAround(cursor: IndexedCursor): Callback = {
      val document = $.state.runNow().document
      val line = document.tasks(cursor.seqIndex).contentString

      def moveOffset(offsetInTask: Int, step: Int): Int = {
        val nextOffset = offsetInTask + step
        if (nextOffset < 0 || nextOffset > line.length) {
          offsetInTask
        } else {
          val currentChar = if (step > 0) line.charAt(offsetInTask) else line.charAt(nextOffset)
          currentChar match {
            case ' ' | '\f' | '\n' | '\r' | '\t' | '\u00A0' | '\u2028' | '\u2029' | '$' => offsetInTask
            case _                                                                      => moveOffset(nextOffset, step)
          }
        }
      }

      val newStartOffset = moveOffset(cursor.offsetInTask, step = -1)
      val newEndOffset = moveOffset(cursor.offsetInTask, step = +1)
      setSelection(
        IndexedSelection(
          cursor.copy(offsetInTask = newStartOffset),
          cursor.copy(offsetInTask = newEndOffset)))
    }

    private def replaceWithHistory(tasksToReplace: Seq[Task],
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

    private def setSelection(selection: IndexedSelection): Callback = LogExceptionsCallback {
      val document = $.state.runNow().document
      def mapToNonCollapsedCursor(cursor: IndexedCursor): IndexedCursor = {
        if (maybeGetTaskElement(cursor).isDefined) {
          cursor
        } else {
          ((cursor.seqIndex + 1) until document.tasks.size)
            .map(IndexedCursor(_, 0))
            .find(maybeGetTaskElement(_).isDefined) match {
            case Some(c) => c
            case None =>
              (0 until cursor.seqIndex).reverse
                .map(IndexedCursor(_, 0))
                .find(maybeGetTaskElement(_).isDefined)
                .get
          }
        }
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

      val start = mapToNonCollapsedCursor(selection.start)
      val end = mapToNonCollapsedCursor(selection.end)
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
  }

  // **************** Helper classes and methods **************** //
  @visibleForTesting private[document] case class Replacement(parts: Seq[Replacement.Part]) {
    def contentString: String = parts.map(_.contentString).mkString
  }
  @visibleForTesting private[document] object Replacement {
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
  @visibleForTesting private[document] case class ClipboardData(htmlText: String, plainText: String)
  @visibleForTesting private[document] def convertToClipboardData(
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

  @visibleForTesting private[document] def clipboardStringToReplacement(
      clipboardString: String,
      baseFormatting: Formatting): Replacement = {
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
          val parsedText = TextWithMarkup.fromHtmlNodes(childNodesWithoutLi)
          for (line <- Splitter.on('\n').omitEmptyStrings().trimResults().split(parsedText.contentString)) {
            partsBuilder.append(
              Replacement.Part(TextWithMarkup(line, baseFormatting), zeroIfNegative(nextRelativeIndentation)))
          }
          childNodesWithoutLi.clear()
        }
      }

      for (node <- nodes) {
        if (containsListItem(node)) {
          pushChildNodesWithoutLi()
          if (nodeIsLi(node)) {
            partsBuilder.append(
              Replacement.Part(
                TextWithMarkup.fromHtmlNodes(Seq(node), baseFormatting),
                zeroIfNegative(nextRelativeIndentation)))
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

  private type Index = Int
  private type AmountCollapsed = Int
  private def applyCollapsedProperty(tasks: Seq[Task]): Stream[(Task, Index, Option[AmountCollapsed])] = {
    def getAmountCollapsed(tasks: Stream[(Task, Index)], collapsedIndentation: Int): Int = {
      tasks.takeWhile(_._1.indentation > collapsedIndentation).size
    }
    def inner(tasks: Stream[(Task, Index)]): Stream[(Task, Index, Option[AmountCollapsed])] = tasks match {
      case (task, i) #:: rest if task.collapsed =>
        val amountCollapsed = getAmountCollapsed(rest, task.indentation)
        (task, i, Some(amountCollapsed)) #:: inner(rest.drop(amountCollapsed))
      case (task, i) #:: rest => (task, i, /* maybeAmountCollapsed = */ None) #:: inner(rest)
      case Stream.Empty       => Stream.Empty
    }
    inner(tasks.zipWithIndex.toStream)
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

  private def maybeGetTaskElement(cursor: IndexedCursor): Option[dom.raw.Element] =
    Option(dom.document.getElementById(s"teli-${cursor.seqIndex}"))

  private def getTaskElement(cursor: IndexedCursor): dom.raw.Element = maybeGetTaskElement(cursor) match {
    case Some(e) => e
    case None    => throw new IllegalStateException(s"Could not find <li> task with seqIndex=${cursor.seqIndex}")
  }

  private def getAnyLinkInSelection(selection: IndexedSelection)(
      implicit document: Document): Option[String] = {
    val cursor = selection.end
    val taskElement = getTaskElement(selection.end)
    val nodeAtCursor: dom.raw.Node = walkDepthFirstPreOrder(taskElement)
      .find {
        case NodeWithOffset(node, offsetSoFar, offsetAtEnd) =>
          offsetSoFar <= cursor.offsetInTask && cursor.offsetInTask <= offsetAtEnd
      }
      .get
      .node

    def nodeAndParents(node: dom.raw.Node): Stream[dom.raw.Node] = node match {
      case `taskElement` => Stream.empty
      case _             => node #:: nodeAndParents(node.parentNode)
    }
    nodeAndParents(nodeAtCursor).collectFirst(node =>
      DomNodeUtils.parseNode(node) match {
        case ParsedNode.A(element) => element.getAttribute("href")
    })
  }

  private def zeroIfNegative(i: Int): Int = if (i < 0) 0 else i

  private case class SingletonFormating(cursor: DetachedCursor, formatting: Formatting)
}
