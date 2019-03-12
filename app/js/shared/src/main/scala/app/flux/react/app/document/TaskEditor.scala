package app.flux.react.app.document

import hydro.common.GuavaReplacement.Splitter
import hydro.common.ScalaUtils.ifThenOption
import hydro.common.ScalaUtils.visibleForTesting
import hydro.common.I18n
import hydro.common.OrderToken
import hydro.common.Tags
import app.flux.react.app.document.KeyCombination._
import app.flux.stores.document.DocumentSelectionStore
import app.flux.stores.document.DocumentStore
import app.models.document.Document.DetachedCursor
import app.models.document.Document.IndexedCursor
import app.models.document.Document.IndexedSelection
import app.models.document.Document
import app.models.document.DocumentEdit
import app.models.document.DocumentEdit.MaskedTaskUpdate
import app.models.document.Task
import app.models.document.TextWithMarkup
import app.models.document.TextWithMarkup.Formatting
import hydro.common.DomNodeUtils
import hydro.common.DomNodeUtils._
import hydro.common.time.Clock
import hydro.flux.react.ReactVdomUtils.^^
import hydro.flux.router.RouterContext
import hydro.flux.stores.StateStore
import hydro.models.access.EntityAccess
import japgolly.scalajs.react._
import japgolly.scalajs.react.raw.SyntheticKeyboardEvent
import japgolly.scalajs.react.vdom.PackageBase.VdomAttr
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom

import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.concurrent.duration._
import scala.scalajs.js

private[document] final class TaskEditor(implicit entityAccess: EntityAccess,
                                         i18n: I18n,
                                         clock: Clock,
                                         documentSelectionStore: DocumentSelectionStore) {

  private val component = ScalaComponent
    .builder[Props](getClass.getSimpleName)
    .initialStateFromProps(props => State(document = props.documentStore.state.document))
    .renderBackend[Backend]
    .componentWillMount(scope => scope.backend.willMount(scope.props, scope.state))
    .componentDidMount(scope => scope.backend.didMount(scope.props, scope.state))
    .componentWillUnmount(scope => scope.backend.willUnmount(scope.props, scope.state))
    .build

  // **************** API ****************//
  def apply(documentStore: DocumentStore)(implicit router: RouterContext): VdomElement = {
    component(Props(documentStore, router))
  }

  // **************** Private inner types ****************//
  private case class Props(documentStore: DocumentStore, router: RouterContext)
  private case class State(document: Document, highlightedTaskIndex: Int = 0)

  private class Backend($ : BackendScope[Props, State]) extends StateStore.Listener {

    private val resizeListener: js.Function1[dom.raw.Event, Unit] = _ => $.forceUpdate.runNow()
    private val editHistory: EditHistory = new EditHistory()
    private var lastSingletonFormating: SingletonFormating = SingletonFormating(
      cursor = DetachedCursor(task = Task.nullInstance, offsetInTask = 0),
      formatting = Formatting.none
    )

    def willMount(props: Props, state: State): Callback = {
      props.documentStore.register(this)
      $.modState(state => state.copy(document = props.documentStore.state.document))
    }

    def didMount(props: Props, state: State): Callback = {
      val selection = documentSelectionStore.getSelection(state.document)
      // Add timeout because scroll to view doesn't seem to work immediately after mount
      js.timers.setTimeout(20.milliseconds) {
        setSelection(selection).runNow()
      }

      dom.window.addEventListener("resize", resizeListener)
      Callback.empty
    }

    def willUnmount(props: Props, state: State): Callback = {
      dom.window.removeEventListener("resize", resizeListener)

      documentSelectionStore
        .setSelection(state.document, IndexedSelection.tupleFromSelection(dom.window.getSelection()))

      props.documentStore.deregister(this)
      Callback.empty
    }

    override def onStateUpdate() = {
      val props = $.props.runNow()
      $.modState(state => state.copy(document = props.documentStore.state.document)).runNow()
    }

    def render(props: Props, state: State): VdomElement = {
      case class RenderedTag(span: VdomElement, style: String)
      def renderTags(tags: Seq[String], seqIndex: Int): Seq[RenderedTag] = tags.zipWithIndex.map {
        case (tag, tagIndex) =>
          val tagId = s"tag-$seqIndex-$tagIndex"
          RenderedTag(
            span = <.span(
              ^^.classes("tag", "label", s"label-${Tags.getBootstrapClassSuffix(tag)}"),
              ^.key := tagId,
              ^.id := tagId),
            style = s"""#$tagId:after {content: "$tag";}"""
          )
      }

      implicit val router = props.router
      <.div(
        ^.contentEditable := true,
        ^.className := "task-editor",
        ^.spellCheck := false,
        VdomAttr("suppressContentEditableWarning") := true,
        ^.onKeyDown ==> handleKeyDown,
        ^.onSelect ==> (_ => updateCursor),
        ^.onPaste ==> handlePaste,
        ^.onCut ==> handleCut,
        ^.onCopy ==> handleCopy,
        ^.style := js.Dictionary("height" -> s"${editorHeightPx}px"),
        <.ul(
          applyCollapsedProperty(state.document.tasks).map {
            case (task, taskIndex, maybeAmountCollapsed) =>
              val nodeType = state.document.tasksOption(taskIndex + 1) match {
                case _ if task.indentation == 0                                => "root"
                case Some(nextTask) if nextTask.indentation > task.indentation => "node"
                case _                                                         => "leaf"
              }
              val renderedTags = renderTags(task.tags, seqIndex = taskIndex)
              val collapsedSuffixStyle = maybeAmountCollapsed.map(amountCollapsed =>
                s"""#teli-$taskIndex:after {content: "  {+ $amountCollapsed}";}""")
              val styleStrings = renderedTags.map(_.style) ++ collapsedSuffixStyle

              (<.li(
                ^.key := s"li-$taskIndex",
                ^.id := s"teli-$taskIndex",
                ^.style := js.Dictionary("marginLeft" -> s"${task.indentation * 50}px"),
                ^^.classes(
                  Seq(nodeType) ++
                    ifThenOption(task.collapsed)("collapsed") ++
                    ifThenOption(state.highlightedTaskIndex == taskIndex)("highlighted")),
                VdomAttr("num") := taskIndex,
                renderedTags.map(_.span).toVdomArray,
                task.content.toVdomNode
              ) +: {
                if (styleStrings.nonEmpty) {
                  Seq(<.styleTag(^.key := s"listyle-$taskIndex", styleStrings.mkString("\n")))
                } else {
                  Seq()
                }
              }).toVdomArray
          }.toVdomArray
        )
      )
    }

    private def editorHeightPx: Int = {
      val windowHeight = if (dom.window.innerHeight > 0) dom.window.innerHeight else dom.window.screen.height
      Integer.max(windowHeight.toInt - 230, 400)
    }

    private def handleCopy(event: ReactEventFromInput): Callback = $.state.map[Unit] { implicit state =>
      event.preventDefault()

      modifyEventClipboardData(event)
    }

    private def handleCut(event: ReactEventFromInput): Callback =
      $.state flatMap { implicit state =>
        $.props flatMap { implicit props =>
          event.preventDefault()

          modifyEventClipboardData(event)

          val selection = IndexedSelection.tupleFromSelection(dom.window.getSelection())

          documentSelectionStore.setSelection(state.document, selection)

          replaceSelection(replacement = Replacement.empty, selection)
        }
      }

    private def modifyEventClipboardData(event: ReactEventFromInput)(implicit state: State): Unit = {
      val selection = IndexedSelection.tupleFromSelection(dom.window.getSelection())
      val document = state.document

      if (selection.start != selection.end) {
        val ClipboardData(htmlText, plainText) = convertToClipboardData(document, selection)

        event.nativeEvent.asInstanceOf[js.Dynamic].clipboardData.setData("text/html", htmlText)
        event.nativeEvent.asInstanceOf[js.Dynamic].clipboardData.setData("text/plain", plainText)
      }
    }

    private def handlePaste(event: ReactEventFromInput): Callback =
      $.state flatMap { implicit state =>
        $.props flatMap { implicit props =>
          event.preventDefault()
          val selection = IndexedSelection.tupleFromSelection(dom.window.getSelection())
          val IndexedSelection(start, end) = selection
          implicit val document = state.document
          val formatting =
            if (lastSingletonFormating.cursor == start.detach) {
              lastSingletonFormating.formatting
            } else {
              document.tasks(start.seqIndex).content.formattingAtCursor(start.offsetInTask)
            }

          documentSelectionStore.setSelection(document, selection)

          replaceSelection(
            replacement =
              clipboardStringToReplacement(getAnyClipboardString(event), baseFormatting = formatting),
            selection)
        }
      }

    private def updateCursor: Callback = {
      val selection = IndexedSelection.tupleFromSelection(dom.window.getSelection())
      $.modState(_.copy(highlightedTaskIndex = selection.end.seqIndex))
    }

    private def handleKeyDown(event: SyntheticKeyboardEvent[_]): Callback =
      $.state flatMap { implicit state =>
        $.props flatMap { implicit props =>
          val selection = IndexedSelection.tupleFromSelection(dom.window.getSelection())
          val IndexedSelection(start, end) = selection
          implicit val document = state.document
          val formatting =
            if (lastSingletonFormating.cursor == start.detach) {
              lastSingletonFormating.formatting
            } else {
              document.tasks(start.seqIndex).content.formattingAtCursor(start.offsetInTask)
            }

          documentSelectionStore.setSelection(document, selection)

          val keyCombination = KeyCombination.fromEvent(event)

          //dom.console.log(s"keyCombination = $keyCombination")

          keyCombination match {
            case CharacterKey(character, /* ctrlOrMeta */ false, /* shift */ _, /* alt */ false) =>
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
                if (selection.isSingleton &&
                    document.tasks(start.seqIndex).collapsed &&
                    start == start.toEndOfTask) {
                  // Pressing enter at the end of a collapsed task --> skip the children
                  val lastCollapsedIndex = selection.includeChildren(collapsedOnly = true).end.seqIndex
                  replaceSelection(
                    replacement = Replacement.newEmptyTask(
                      indentationRelativeToCurrent =
                        document.tasks(start.seqIndex).indentation
                          - document.tasks(lastCollapsedIndex).indentation),
                    IndexedSelection.singleton(IndexedCursor.atEndOfTask(lastCollapsedIndex))
                  )
                } else {
                  replaceSelection(replacement = Replacement.newEmptyTask(), IndexedSelection(start, end))
                }
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

            case SpecialKey(Delete, /* ctrlOrMeta */ true, /* shift */ true, /* alt */ false) => // Delete rest of line
              event.preventDefault()
              replaceSelection(replacement = Replacement.empty, IndexedSelection(start, end.toEndOfTask))

            case SpecialKey(Tab, /* ctrlOrMeta */ false, /* shift */ _, /* alt */ false) =>
              event.preventDefault()
              val indentIncrease = if (keyCombination.shift) -1 else 1
              // Don't indent children if task is empty
              val updateChildren = !(selection.isSingleton && document.tasks(start.seqIndex).content.isEmpty)
              updateTasksInSelection(selection, updateChildren = updateChildren) { task =>
                MaskedTaskUpdate.fromFields(
                  task,
                  indentation = zeroIfNegative(task.indentation + indentIncrease))
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
            case CharacterKey('C', /* ctrlOrMeta */ false, /* shift */ true, /* alt */ true) |
                CharacterKey('C', /* ctrlOrMeta */ true, /* shift */ true, /* alt */ false) =>
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

            // Select task
            case CharacterKey('j', /* ctrlOrMeta */ true, /* shift */ false, /* alt */ false) =>
              event.preventDefault()
              setSelection(IndexedSelection(start.toStartOfTask, start.toEndOfTask))

            // Delete task
            case CharacterKey('d', /* ctrlOrMeta */ true, /* shift */ false, /* alt */ false) =>
              event.preventDefault()
              removeTasks(selection.includeChildren().seqIndices)

            // Duplicate task
            case CharacterKey('B', /* ctrlOrMeta */ true, /* shift */ true, /* alt */ false) =>
              event.preventDefault()
              duplicateTasks(
                selection.includeChildren(collapsedOnly = true).seqIndices,
                selectionBeforeEdit = selection)

            // Move tasks up
            case SpecialKey(ArrowUp, /* ctrlOrMeta */ false, /* shift */ false, /* alt */ true) =>
              event.preventDefault()
              moveTasksInSeq(selection, direction = -1)

            // Move tasks down
            case SpecialKey(ArrowDown, /* ctrlOrMeta */ false, /* shift */ false, /* alt */ true) =>
              event.preventDefault()
              moveTasksInSeq(selection, direction = +1)

            // Expand tasks
            case CharacterKey('=' | '+', /* ctrlOrMeta */ true, /* shift */ false, /* alt */ false) =>
              event.preventDefault()
              updateTasksInSelection(selection, updateChildren = false) { task =>
                MaskedTaskUpdate.fromFields(task, collapsed = false)
              }

            // Collapse tasks
            case CharacterKey('-', /* ctrlOrMeta */ true, /* shift */ false, /* alt */ false) =>
              event.preventDefault()
              updateTasksInSelection(selection, updateChildren = false) { task =>
                MaskedTaskUpdate.fromFields(task, collapsed = true)
              }

            // Convert to upper case
            case CharacterKey('U', /* ctrlOrMeta */ true, /* shift */ true, /* alt */ false) =>
              event.preventDefault()
              updateCharactersInSelection(selection, _.toUpperCase)

            // Convert to lower case
            case CharacterKey('L', /* ctrlOrMeta */ true, /* shift */ true, /* alt */ false) =>
              event.preventDefault()
              updateCharactersInSelection(selection, _.toLowerCase)

            // Edit tags
            case CharacterKey('T', /* ctrlOrMeta */ false, /* shift */ true, /* alt */ true) =>
              event.preventDefault()
              editTagsInTasks(selection)

            case _ =>
              Callback.empty
          }
        }
      }

    private def applyHistoryEdit(maybeEdit: Option[EditHistory.Edit])(implicit props: Props): Callback =
      maybeEdit match {
        case None => Callback.empty
        case Some(edit) =>
          val documentStore = props.documentStore
          val newDocument = documentStore.applyEditWithoutCallingListeners(edit.documentEdit)
          $.modState(
            _.copy(document = newDocument),
            Callback.empty.flatMap(_ => setSelection(edit.selectionAfterEdit.attachToDocument(newDocument)))
          )
      }

    private def replaceSelection(replacement: Replacement, selectionBeforeEdit: IndexedSelection)(
        implicit state: State,
        props: Props): Callback = {
      val IndexedSelection(start, end) = selectionBeforeEdit
      implicit val oldDocument = state.document

      val newOrderTokens = {
        val previousTask = oldDocument.tasksOption(start.seqIndex - 1)
        val nextTask = oldDocument.tasksOption(end.seqIndex + 1)
        OrderToken.evenlyDistributedValuesBetween(
          numValues = replacement.parts.length,
          lowerExclusive = previousTask.map(_.orderToken),
          higherExclusive = nextTask.map(_.orderToken)
        )
      }

      val firstTask = oldDocument.tasks(start.seqIndex)
      val replacementIndexMatchedToFirstTask = {
        if (replacement.parts.size > 1 && replacement.parts.head.contentString.isEmpty && start.offsetInTask == 0)
          1
        else 0
      }

      val removedTasks = oldDocument.tasksIn(selectionBeforeEdit).filter(_.id != firstTask.id)
      val taskUpdates = mutable.Buffer[MaskedTaskUpdate]()
      val addedTasks = mutable.Buffer[Task]()

      for (((replacementPart, newOrderToken), i) <- (replacement.parts zip newOrderTokens).zipWithIndex)
        yield {
          def ifIndexOrEmpty(index: Int)(tags: TextWithMarkup): TextWithMarkup =
            if (i == index) tags else TextWithMarkup.empty
          val newContent = ifIndexOrEmpty(0)(firstTask.content.sub(0, start.offsetInTask)) +
            replacementPart.content +
            ifIndexOrEmpty(replacement.parts.length - 1)(
              oldDocument.tasks(end.seqIndex).content.sub(end.offsetInTask))
          if (i == replacementIndexMatchedToFirstTask) {
            taskUpdates.append(
              MaskedTaskUpdate.fromFields(
                firstTask,
                content = newContent,
                orderToken = newOrderToken,
                indentation = firstTask.indentation + replacementPart.indentationRelativeToCurrent))
          } else {
            addedTasks.append(
              Task.withRandomId(
                content = newContent,
                orderToken = newOrderToken,
                indentation = firstTask.indentation + replacementPart.indentationRelativeToCurrent,
                collapsed = false,
                delayedUntil = None,
                tags = Seq(),
              ))
          }
        }

      replaceWithHistory(
        edit = DocumentEdit.Reversible(
          removedTasks = removedTasks,
          addedTasks = addedTasks.toVector,
          taskUpdates = taskUpdates.toVector),
        selectionBeforeEdit = selectionBeforeEdit,
        selectionAfterEdit = IndexedSelection.singleton(
          (start proceedNTasks (replacement.parts.length - 1)) plusOffset replacement.parts.last.contentString.length),
        replacementString = replacement.contentString
      )
    }

    private def removeTasks(taskIndices: Range)(implicit state: State, props: Props): Callback = {
      implicit val oldDocument = state.document

      val removedTasks = for (i <- taskIndices) yield oldDocument.tasks(i)
      val addedTasks =
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
        edit = DocumentEdit.Reversible(removedTasks = removedTasks, addedTasks = addedTasks),
        selectionBeforeEdit = IndexedSelection(
          IndexedCursor.atStartOfTask(taskIndices.head),
          IndexedCursor.atEndOfTask(taskIndices.last)),
        selectionAfterEdit = IndexedSelection.singleton(
          IndexedCursor.atStartOfTask(
            if (oldDocument.tasks.size > taskIndices.head + taskIndices.size) taskIndices.head
            else if (taskIndices.head == 0) 0
            else taskIndices.head - 1))
      )
    }

    private def duplicateTasks(taskIndices: Range, selectionBeforeEdit: IndexedSelection)(
        implicit state: State,
        props: Props): Callback = {
      implicit val oldDocument = state.document

      val newOrderTokens = {
        val taskBefore = oldDocument.tasksOption(taskIndices.last)
        val taskAfter = oldDocument.tasksOption(taskIndices.last + 1)
        OrderToken.evenlyDistributedValuesBetween(
          numValues = taskIndices.size,
          lowerExclusive = taskBefore.map(_.orderToken),
          higherExclusive = taskAfter.map(_.orderToken)
        )
      }
      val addedTasks = for ((i, orderToken) <- taskIndices zip newOrderTokens)
        yield {
          val taskToCopy = oldDocument.tasks(i)
          Task.withRandomId(
            content = taskToCopy.content,
            orderToken = orderToken,
            indentation = taskToCopy.indentation,
            collapsed = taskToCopy.collapsed,
            delayedUntil = taskToCopy.delayedUntil,
            tags = taskToCopy.tags,
          )
        }

      replaceWithHistory(
        edit = DocumentEdit.Reversible(addedTasks = addedTasks),
        selectionBeforeEdit = selectionBeforeEdit,
        selectionAfterEdit = IndexedSelection(
          selectionBeforeEdit.start.plusTasks(taskIndices.size),
          selectionBeforeEdit.end.plusTasks(taskIndices.size))
      )
    }

    private def updateTasksInSelection(selection: IndexedSelection, updateChildren: Boolean)(
        taskUpdate: Task => MaskedTaskUpdate)(implicit state: State, props: Props): Callback = {
      implicit val oldDocument = state.document
      val updateSelection = if (updateChildren) selection.includeChildren() else selection
      val taskUpdates = for (task <- oldDocument.tasksIn(updateSelection)) yield taskUpdate(task)

      replaceWithHistory(
        edit = DocumentEdit.Reversible(taskUpdates = taskUpdates),
        selectionBeforeEdit = selection,
        selectionAfterEdit = selection
      )
    }

    private def updateCharactersInSelection(
        selection: IndexedSelection,
        characterTransform: String => String)(implicit state: State, props: Props): Callback = {
      implicit val oldDocument = state.document

      val taskUpdates = for (task <- oldDocument.tasksIn(selection))
        yield
          MaskedTaskUpdate.fromFields(
            originalTask = task,
            content = task.content.withTransformedCharacters(
              beginOffset = selection.startOffsetInTask(task),
              endOffset = selection.endOffsetInTask(task),
              characterTransform = characterTransform
            )
          )
      replaceWithHistory(
        edit = DocumentEdit.Reversible(taskUpdates = taskUpdates),
        selectionBeforeEdit = selection,
        selectionAfterEdit = selection
      )
    }

    private def editTagsInTasks(selection: IndexedSelection)(implicit state: State,
                                                             props: Props): Callback = {
      implicit val document = state.document

      class CancelException extends Exception
      def tagsDialog(defaultTags: Seq[String]): Seq[String] = {
        val title = if (defaultTags.isEmpty) "Add tags" else "Edit tags"
        val result = dom.window.prompt(title, defaultTags.mkString(", "))
        result match {
          case null => throw new CancelException
          case s    => Splitter.on(',').trimResults().split(s).filter(Tags.isValidTag)
        }
      }

      def replaceTags(indexedSelection: IndexedSelection,
                      tagsToRemove: Seq[String],
                      tagsToAdd: Seq[String]): Callback = {
        val taskUpdates = for (task <- document.tasksIn(selection))
          yield
            MaskedTaskUpdate.fromFields(
              originalTask = task,
              tags = task.tags.filterNot(tagsToRemove contains _) ++ tagsToAdd)
        replaceWithHistory(
          edit = DocumentEdit.Reversible(taskUpdates = taskUpdates.filter(!_.isNoOp)),
          selectionBeforeEdit = selection,
          selectionAfterEdit = selection
        )
      }

      val currentTags: Seq[String] = {
        val tasks = for (i <- selection.seqIndices) yield document.tasks(i)
        tasks.map(_.tags).reduce(_ intersect _)
      }

      try {
        val newTags = tagsDialog(currentTags)
        replaceTags(selection, currentTags, newTags)
      } catch {
        case _: CancelException => Callback.empty
      }
    }

    private def toggleFormatting(
        updateFunc: (Formatting, Boolean) => Formatting,
        selection: IndexedSelection,
        formattingAtStart: Formatting)(implicit state: State, props: Props): Callback = {
      implicit val document = state.document
      val IndexedSelection(start, end) = selection

      def toggleFormattingInternal(start: IndexedCursor, end: IndexedCursor): Callback = {
        def setFormatting(tasks: Seq[Task], value: Boolean): Seq[MaskedTaskUpdate] =
          for (task <- tasks)
            yield
              MaskedTaskUpdate.fromFields(
                originalTask = task,
                content = task.content.withFormatting(
                  beginOffset = if (task == tasks.head) start.offsetInTask else 0,
                  endOffset = if (task == tasks.last) end.offsetInTask else task.contentString.length,
                  formatting => updateFunc(formatting, value)
                )
              )

        val oldDocument = state.document

        val taskUpdates = {
          val candidate = setFormatting(oldDocument.tasksIn(selection), value = true)
          if (candidate.forall(_.isNoOp)) {
            setFormatting(oldDocument.tasksIn(selection), value = false)
          } else {
            candidate
          }
        }

        replaceWithHistory(
          edit = DocumentEdit.Reversible(taskUpdates = taskUpdates),
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

    private def editLink(originalSelection: IndexedSelection)(implicit state: State,
                                                              props: Props): Callback = {
      implicit val oldDocument = state.document

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
        val taskUpdates = for (task <- oldDocument.tasksIn(selection))
          yield
            MaskedTaskUpdate.fromFields(
              originalTask = task,
              content = task.content
                .withFormatting(
                  beginOffset = selection.startOffsetInTask(task),
                  endOffset = selection.endOffsetInTask(task),
                  updateFunc = _.copy(link = newLink)
                )
            )

        replaceWithHistory(
          edit = DocumentEdit.Reversible(taskUpdates = taskUpdates),
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

    private def moveTasksInSeq(selectionBeforeEdit: IndexedSelection,
                               direction: Int)(implicit state: State, props: Props): Callback = {
      implicit val oldDocument = state.document
      val selectionWithChildren = selectionBeforeEdit.includeChildren()
      val IndexedSelection(start, end) = selectionWithChildren

      val seqIndexMovement = {
        val indexThatIsHoppedOver = if (direction < 0) start.seqIndex - 1 else end.seqIndex + 1
        oldDocument.familyTreeRange(
          anyMemberSeqIndex = indexThatIsHoppedOver,
          rootParentIndentation =
            selectionWithChildren.seqIndices.map(i => oldDocument.tasks(i).indentation).min) match {
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
        val newOrderTokens = {
          OrderToken.evenlyDistributedValuesBetween(
            numValues = selectionWithChildren.seqIndices.length,
            lowerExclusive = task1.map(_.orderToken),
            higherExclusive = task2.map(_.orderToken)
          )
        }

        val taskUpdates =
          for ((oldTask, newOrderToken) <- oldDocument.tasksIn(selectionWithChildren) zip newOrderTokens)
            yield MaskedTaskUpdate.fromFields(originalTask = oldTask, orderToken = newOrderToken)

        replaceWithHistory(
          edit = DocumentEdit.Reversible(taskUpdates = taskUpdates),
          selectionBeforeEdit = selectionBeforeEdit,
          selectionAfterEdit = IndexedSelection(
            selectionBeforeEdit.start.copy(seqIndex = selectionBeforeEdit.start.seqIndex + seqIndexMovement),
            selectionBeforeEdit.end.copy(seqIndex = selectionBeforeEdit.end.seqIndex + seqIndexMovement)
          )
        )
      }
    }

    private def selectExtendedWordAround(cursor: IndexedCursor)(implicit state: State): Callback = {
      val document = state.document
      val taskContent = document.tasks(cursor.seqIndex).contentString

      def moveOffset(offsetInTask: Int, step: Int): Int = {
        val nextOffset = offsetInTask + step
        if (nextOffset < 0 || nextOffset > taskContent.length) {
          offsetInTask
        } else {
          val currentChar = if (step > 0) taskContent.charAt(offsetInTask) else taskContent.charAt(nextOffset)
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

    private def replaceWithHistory(
        edit: DocumentEdit.Reversible,
        selectionBeforeEdit: IndexedSelection,
        selectionAfterEdit: IndexedSelection,
        replacementString: String = "")(implicit state: State, props: Props): Callback = {

      val documentStore = props.documentStore
      val oldDocument = state.document
      val newDocument = documentStore.applyEditWithoutCallingListeners(edit)

      $.modState(
        _.copy(document = newDocument),
        Callback.empty.flatMap { _ =>
          editHistory.addEdit(
            documentEdit = edit,
            selectionBeforeEdit = selectionBeforeEdit.detach(oldDocument),
            selectionAfterEdit = selectionAfterEdit.detach(newDocument),
            replacementString = replacementString
          )
          setSelection(selectionAfterEdit)
        }
      )
    }

    private def setSelection(selection: IndexedSelection): Callback = $.state.map[Unit] { state =>
      val document = state.document
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
      clipboardData: ClipboardData,
      baseFormatting: Formatting): Replacement = {
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

    if (clipboardData.htmlText.nonEmpty) {
      val htmlElement = {
        val resultHolder = dom.document.createElement("span")
        resultHolder.innerHTML = clipboardData.htmlText
        resultHolder
      }
      addPastedText(Seq(htmlElement), nextRelativeIndentation = -1)
    } else {
      Splitter.on('\n').split(clipboardData.plainText).foreach { line =>
        partsBuilder.append(
          Replacement.Part(
            content = TextWithMarkup(line, formatting = baseFormatting),
            indentationRelativeToCurrent = 0))
      }
    }
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

  private def getAnyClipboardString(event: ReactEventFromInput): ClipboardData = ClipboardData(
    htmlText =
      event.nativeEvent.asInstanceOf[js.Dynamic].clipboardData.getData("text/html").asInstanceOf[String],
    plainText =
      event.nativeEvent.asInstanceOf[js.Dynamic].clipboardData.getData("text/plain").asInstanceOf[String],
  )

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

  private case class SingletonFormating(cursor: DetachedCursor, formatting: Formatting)

  @visibleForTesting private[document] case class ClipboardData(htmlText: String, plainText: String)
}
