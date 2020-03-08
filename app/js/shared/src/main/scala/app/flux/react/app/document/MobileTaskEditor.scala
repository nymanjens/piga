package app.flux.react.app.document

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent.duration._
import app.flux.react.app.document.TaskEditorUtils.TaskInSeq
import app.flux.react.app.document.TaskEditorUtils.applyCollapsedProperty
import app.flux.react.uielements.ResizingTextArea
import app.flux.react.uielements.ResizingTextArea.Fixed
import app.flux.react.uielements.ResizingTextArea.ScaleWithInput
import app.flux.router.AppPages
import app.flux.stores.document.DocumentSelectionStore
import app.flux.stores.document.DocumentStore
import app.flux.stores.document.DocumentStoreFactory
import app.models.document.Document
import app.models.document.Document.IndexedCursor
import app.models.document.Document.IndexedSelection
import app.models.document.DocumentEdit
import app.models.document.DocumentEdit.MaskedTaskUpdate
import app.models.document.Task
import app.models.document.TextWithMarkup
import app.models.user.User
import hydro.common.I18n
import hydro.common.ScalaUtils.ifThenOption
import hydro.common.Tags
import hydro.common.time.Clock
import hydro.common.CollectionUtils
import hydro.common.GuavaReplacement
import hydro.common.GuavaReplacement.LoadingCache
import hydro.common.OrderToken
import hydro.common.Annotations.visibleForTesting
import hydro.flux.react.HydroReactComponent
import hydro.flux.react.ReactVdomUtils.<<
import hydro.flux.react.ReactVdomUtils.^^
import hydro.flux.react.uielements.Bootstrap
import hydro.flux.react.uielements.Bootstrap.Size
import hydro.flux.react.uielements.Bootstrap.Variant
import hydro.flux.react.uielements.BootstrapTags
import hydro.flux.router.RouterContext
import hydro.models.access.EntityAccess
import japgolly.scalajs.react._
import japgolly.scalajs.react.raw.SyntheticEvent
import japgolly.scalajs.react.raw.SyntheticKeyboardEvent
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom

import scala.async.Async.async
import scala.async.Async.await
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.scalajs.js

private[document] final class MobileTaskEditor(
    implicit entityAccess: EntityAccess,
    user: User,
    i18n: I18n,
    clock: Clock,
    documentSelectionStore: DocumentSelectionStore,
    documentStoreFactory: DocumentStoreFactory,
    editHistory: EditHistory,
) extends HydroReactComponent {

  // **************** API ****************//
  def apply(documentStore: DocumentStore)(implicit router: RouterContext): VdomElement = {
    component(Props(documentStore, router))
  }

  // **************** Implementation of HydroReactComponent methods ****************//
  override protected val config = ComponentConfig(backendConstructor = new Backend(_), initialState = State())
    .withStateStoresDependencyFromProps { props =>
      val store = props.documentStore
      StateStoresDependency(store, _.copyFromStore(store))
    }

  // **************** Implementation of HydroReactComponent types ****************//
  protected case class Props(documentStore: DocumentStore, router: RouterContext)
  protected case class State(
      inEditMode: Boolean = false,
      document: Document = Document.nullInstance,
      pendingTaskIds: Set[Long] = Set(),
      highlightedTaskIndex: Int = 0,
  ) {
    def copyFromStore(documentStore: DocumentStore): State =
      copy(document = documentStore.state.document, pendingTaskIds = documentStore.state.pendingTaskIds)

    lazy val highlightedTask: Task = document.tasks(highlightedTaskIndex)
    def startOfHighlightedTask: IndexedSelection =
      IndexedSelection.singleton(IndexedCursor.atStartOfTask(highlightedTaskIndex))

  }

  protected class Backend($ : BackendScope[Props, State]) extends BackendBase($) with DidMount {

    private val taskIdToInputRef: LoadingCache[Long, ResizingTextArea.Reference] =
      LoadingCache.fromLoader(_ => ResizingTextArea.ref())

    override def didMount(props: Props, state: State): Callback = {
      val selection = documentSelectionStore.getSelection(state.document.id)
      val taskIndex = selection.start.seqIndex

      if (state.document.tasksOption(taskIndex).isDefined) {
        $.modState(
          _.copy(highlightedTaskIndex = taskIndex),
          Callback {
            val taskToFocus = state.document.tasks(taskIndex)
            taskIdToInputRef.get(taskToFocus.id)().focus()
          }
        )
      } else {
        Callback.empty
      }
    }

    override def render(props: Props, state: State): VdomElement = {
      implicit val router = props.router
      implicit val implicitState = state
      implicit val implicitProps = props
      <.span(
        ^.className := "mobile-task-editor",
        <.ul(
          applyCollapsedProperty(state.document.tasks).map {
            case TaskInSeq(task, taskIndex, maybeAmountCollapsed, isRoot, isLeaf) =>
              val isHighlighted = state.highlightedTaskIndex == taskIndex
              val canBeEdited = task.content.isPlainText && !task.content.containsLink
              val isReadOnly =
                if (state.inEditMode) {
                  !(canBeEdited && isHighlighted)
                } else {
                  true
                }

              <.li(
                ^.key := s"li-${task.id}",
                ^.id := s"teli-$taskIndex",
                ^.style := js.Dictionary("marginLeft" -> s"${task.indentation * 20}px"),
                ^^.classes(
                  Seq() ++
                    ifThenOption(isRoot)("root") ++
                    ifThenOption(isLeaf)("leaf") ++
                    ifThenOption(task.contentString.isEmpty)("empty-task") ++
                    ifThenOption(task.collapsed)("collapsed") ++
                    ifThenOption(isHighlighted)("highlighted") ++
                    ifThenOption(state.pendingTaskIds contains task.id)("modification-pending") ++
                    ifThenOption(task.lastContentModifierUserId != user.id)("modified-by-other-user") ++
                    ifThenOption(isReadOnly)("read-only")),
                ^.onClick --> selectTask(task),
                task.tags.zipWithIndex.map {
                  case (tag, tagIndex) =>
                    <.div( // This is a holder for the label to avoid tags to be affected by the surrounding flex box
                      ^.key := tagIndex,
                      ^.className := "tag-holder",
                      Bootstrap.Label(BootstrapTags.toStableVariant(tag))(
                        ^.className := "tag",
                        tag,
                      ),
                    )
                }.toVdomArray, {
                  if (isReadOnly) readonlyTask(task)
                  else plainTextInput(task)
                },
                <<.ifThen(isHighlighted) {
                  editModeToggleButton(canBeEdited)
                },
                <<.ifDefined(maybeAmountCollapsed) { amountCollapsed =>
                  <.div(
                    ^.className := "collapsed-suffix",
                    s" {+ $amountCollapsed}"
                  )
                },
              )
          }.toVdomArray
        ),
        editButtons
      )
    }

    private def plainTextInput(task: Task)(implicit props: Props, state: State): VdomNode = {
      ResizingTextArea(
        resizeStrategy = if (state.highlightedTask == task) ScaleWithInput else Fixed(numberOfRows = 1),
        ref = taskIdToInputRef.get(task.id)
      )(
        ^.key := task.id,
        ^.value := task.contentString,
        ^.spellCheck := false,
        ^.autoFocus := true,
        ^.onChange ==> { (event: ReactEventFromInput) =>
          onPlainTextChange(newContent = event.target.value, originalTask = task)
        },
        ^.onKeyDown ==> handleKeyDown,
      )
    }

    private def readonlyTask(task: Task): VdomNode = {
      <.span(
        ^.className := "readonly-task",
        task.content.toVdomNode,
      )
    }

    private def editModeToggleButton(canBeEdited: Boolean)(implicit state: State): VdomNode = {
      <.span(
        ^.className := "edit-mode-toggle",
        if (state.inEditMode) {
          Bootstrap.Button(Variant.primary, Size.xs)(
            ^.onClick --> toggleInEditMode(),
            Bootstrap.FontAwesomeIcon("check"),
          )
        } else {
          Bootstrap.Button(Variant.primary, Size.xs)(
            ^.onClick --> toggleInEditMode(),
            ^.disabled := !canBeEdited,
            Bootstrap.FontAwesomeIcon("pencil"),
          )
        },
      )
    }

    private def editButtons(implicit props: Props, state: State): VdomNode = <.div(
      ^.className := "edit-buttons",
      Bootstrap.ButtonGroup(
        // Create empty
        Bootstrap.Button(Variant.info)(
          ^.onClick --> creatEmptyTaskUnderHighlighted(),
          Bootstrap.FontAwesomeIcon("calendar-o"),
        ),
        // Delete
        Bootstrap.Button(Variant.info)(
          ^.onClick --> removeHighlightedTask(),
          ^.disabled := state.document.tasks.size == 1,
          Bootstrap.FontAwesomeIcon("trash-o"),
        ),
      ),
      Bootstrap.ButtonGroup(
        // Move up
        Bootstrap.Button(Variant.info)(
          ^.onClick --> moveHighlightedTask(direction = -1),
          ^.disabled := state.highlightedTaskIndex == 0,
          Bootstrap.FontAwesomeIcon("chevron-up"),
        ),
        // Move down
        Bootstrap.Button(Variant.info)(
          ^.onClick --> moveHighlightedTask(direction = +1),
          ^.disabled := state.highlightedTaskIndex == state.document.tasks.size - 1,
          Bootstrap.FontAwesomeIcon("chevron-down"),
        ),
        // Dedent
        Bootstrap.Button(Variant.info)(
          ^.onClick --> indentHighlightedTask(indentIncrease = -1),
          ^.disabled := state.highlightedTask.indentation == 0,
          Bootstrap.FontAwesomeIcon("dedent"),
        ),
        // Indent
        Bootstrap.Button(Variant.info)(
          ^.onClick --> indentHighlightedTask(indentIncrease = +1),
          Bootstrap.FontAwesomeIcon("indent"),
        ),
        // Expand/collapse
        Bootstrap.Button(Variant.info)(
          ^.onClick --> toggleCollapseOnHighlightedTask(),
          if (state.highlightedTask.collapsed) Bootstrap.Glyphicon("collapse-down")
          else Bootstrap.Glyphicon("expand"),
        ),
      ),
      Bootstrap.ButtonGroup(
        // Undo
        Bootstrap.Button(Variant.info)(
          ^.onClick --> applyHistoryEdit(editHistory.undo()),
          ^.disabled := !editHistory.canUndo,
          Bootstrap.FontAwesomeIcon("rotate-left"),
        ),
        // Redo
        Bootstrap.Button(Variant.info)(
          ^.onClick --> applyHistoryEdit(editHistory.redo()),
          ^.disabled := !editHistory.canRedo,
          Bootstrap.FontAwesomeIcon("rotate-right"),
        ),
      ),
    )

    private def onPlainTextChange(newContent: String, originalTask: Task)(
        implicit state: State,
        props: Props,
    ): Callback = {
      replaceWithHistory(
        edit = DocumentEdit.Reversible(
          taskUpdates = Seq(
            MaskedTaskUpdate.fromFields(originalTask = originalTask, content = TextWithMarkup(newContent)))),
        replacementString =
          deriveReplacementString(oldContent = originalTask.contentString, newContent = newContent),
      )
    }

    // Intercept newline to add a new task
    private def handleKeyDown(event: SyntheticKeyboardEvent[_])(
        implicit state: State,
        props: Props,
    ): Callback = {
      event.key match {
        case "Enter" =>
          event.preventDefault()
          creatEmptyTaskUnderHighlighted()
        case _ =>
          Callback.empty
      }
    }

    private def selectTask(task: Task)(implicit state: State): Callback = {
      $.modState { state =>
        state.document.maybeIndexOf(task.id, orderTokenHint = task.orderToken) match {
          case None => state
          case Some(taskIndex) =>
            documentSelectionStore.setSelection(
              documentId = state.document.id,
              IndexedSelection.atStartOfTask(taskIndex))
            state.copy(highlightedTaskIndex = taskIndex)
        }
      }
    }

    private def creatEmptyTaskUnderHighlighted()(implicit state: State, props: Props): Callback = {
      implicit val oldDocument = state.document

      val insertIndex =
        IndexedSelection
          .atStartOfTask(state.highlightedTaskIndex)
          .includeChildren(collapsedOnly = true)
          .end
          .seqIndex

      for {
        // Force edit mode
        _ <- $.modState(state => state.copy(inEditMode = true))

        __ <- replaceWithHistory(
          edit = DocumentEdit.Reversible(
            addedTasks = Seq(Task.withRandomId(
              content = TextWithMarkup.empty,
              orderToken = OrderToken.middleBetween(
                oldDocument.tasksOption(insertIndex).map(_.orderToken),
                oldDocument.tasksOption(insertIndex + 1).map(_.orderToken)),
              indentation = state.highlightedTask.indentation,
              collapsed = false,
              delayedUntil = None,
              tags = Seq()
            ))),
          highlightedTaskIndexAfterEdit = insertIndex + 1,
          focusHighlightedTaskAfterEdit = true,
        )
      } yield (): Unit

    }

    private def toggleInEditMode(): Callback = {
      $.modState(state => state.copy(inEditMode = !state.inEditMode))
    }

    private def removeHighlightedTask()(implicit state: State, props: Props): Callback = {
      implicit val oldDocument = state.document

      // Don't delete children if task is empty
      val updateChildren = state.highlightedTask.content.nonEmpty

      val indicesToRemove =
        if (updateChildren)
          IndexedSelection.atStartOfTask(state.highlightedTaskIndex).includeChildren().seqIndices
        else Seq(state.highlightedTaskIndex)
      val removedTasks = for (i <- indicesToRemove) yield oldDocument.tasks(i)
      val addedTasks =
        if (oldDocument.tasks.size > indicesToRemove.size) Seq()
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
        highlightedTaskIndexAfterEdit =
          if (oldDocument.tasks.size > indicesToRemove.head + indicesToRemove.size) indicesToRemove.head
          else if (indicesToRemove.head == 0) 0
          else indicesToRemove.head - 1
      )
    }

    private def indentHighlightedTask(indentIncrease: Int)(implicit state: State, props: Props): Callback = {
      implicit val oldDocument = state.document

      // Don't indent children if task is empty
      val updateChildren = state.highlightedTask.content.nonEmpty

      val taskIndicesToUpdate =
        if (updateChildren)
          IndexedSelection.atStartOfTask(state.highlightedTaskIndex).includeChildren().seqIndices
        else Seq(state.highlightedTaskIndex)
      val taskUpdates = for (index <- taskIndicesToUpdate) yield {
        val task = oldDocument.tasks(index)
        MaskedTaskUpdate.fromFields(task, indentation = zeroIfNegative(task.indentation + indentIncrease))
      }

      replaceWithHistory(
        edit = DocumentEdit.Reversible(taskUpdates = taskUpdates),
        focusHighlightedTaskAfterEdit = true,
      )
    }

    private def toggleCollapseOnHighlightedTask()(implicit state: State, props: Props): Callback = {
      implicit val oldDocument = state.document

      val taskUpdate =
        MaskedTaskUpdate.fromFields(state.highlightedTask, collapsed = !state.highlightedTask.collapsed)

      replaceWithHistory(
        edit = DocumentEdit.Reversible(taskUpdates = Seq(taskUpdate)),
        focusHighlightedTaskAfterEdit = false,
      )
    }

    private def moveHighlightedTask(direction: Int)(implicit state: State, props: Props): Callback = {
      implicit val oldDocument = state.document
      val selectionWithChildren = IndexedSelection.atStartOfTask(state.highlightedTaskIndex).includeChildren()
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
          highlightedTaskIndexAfterEdit = state.highlightedTaskIndex + seqIndexMovement,
          focusHighlightedTaskAfterEdit = true,
        )
      }
    }

    private def replaceWithHistory(
        edit: DocumentEdit.Reversible,
        highlightedTaskIndexAfterEdit: Int = -1,
        focusHighlightedTaskAfterEdit: Boolean = false,
        replacementString: String = "",
    )(implicit oldState: State, props: Props): Callback = {

      val actualHighlightedTaskIndexAfterEdit =
        if (highlightedTaskIndexAfterEdit == -1) oldState.highlightedTaskIndex
        else highlightedTaskIndexAfterEdit
      val documentStore = props.documentStore
      val oldDocument = oldState.document
      documentStore.applyEditWithoutCallingListeners(edit)
      val newDocument = documentStore.state.document

      $.modState(
        _.copyFromStore(documentStore).copy(highlightedTaskIndex = actualHighlightedTaskIndexAfterEdit),
        Callback {
          editHistory.addEdit(
            documentId = oldState.document.id,
            documentEdit = edit,
            selectionBeforeEdit =
              IndexedSelection.atStartOfTask(oldState.highlightedTaskIndex).detach(oldDocument),
            selectionAfterEdit =
              IndexedSelection.atStartOfTask(actualHighlightedTaskIndexAfterEdit).detach(newDocument),
            replacementString = replacementString
          )

          if (focusHighlightedTaskAfterEdit) {
            val taskToFocus = newDocument.tasks(actualHighlightedTaskIndexAfterEdit)
            taskIdToInputRef.get(taskToFocus.id)().focus()
          }

          documentSelectionStore.setSelection(
            documentId = oldState.document.id,
            IndexedSelection.atStartOfTask(actualHighlightedTaskIndexAfterEdit))
        }
      )
    }

    private def applyHistoryEdit(maybeEdit: Option[EditHistory.Edit])(
        implicit props: Props,
        state: State,
    ): Callback =
      maybeEdit match {
        case None => Callback.empty
        case Some(edit) if edit.documentId == state.document.id =>
          val documentStore = props.documentStore
          documentStore.applyEditWithoutCallingListeners(edit.documentEdit)
          val newDocument = documentStore.state.document
          val newHighlightedTaskIndex = edit.selectionAfterEdit.attachToDocument(newDocument).start.seqIndex
          $.modState(
            _.copyFromStore(documentStore).copy(highlightedTaskIndex = newHighlightedTaskIndex),
            Callback {
              documentSelectionStore.setSelection(
                documentId = state.document.id,
                IndexedSelection.atStartOfTask(newHighlightedTaskIndex))
            }
          )
        case Some(edit) if edit.documentId != state.document.id =>
          Callback.future {
            async {
              val otherDocumentStore = await(documentStoreFactory.create(edit.documentId))
              otherDocumentStore.applyEditWithoutCallingListeners(edit.documentEdit)
              val newOtherDocument = otherDocumentStore.state.document
              documentSelectionStore.setSelection(
                edit.documentId,
                edit.selectionAfterEdit.attachToDocument(newOtherDocument))
              props.router.setPage(AppPages.TaskList(documentId = edit.documentId))
              Callback.empty
            }
          }
      }
  }

  @visibleForTesting
  private[document] def deriveReplacementString(oldContent: String, newContent: String): String = {
    // Note: This is a heuristic. We only handle the case where a string was attached at the end of the line
    // TODO: Also cope with a single character inserted somewhere else

    if (newContent startsWith oldContent) {
      newContent stripPrefix oldContent
    } else {
      ""
    }
  }

  private def zeroIfNegative(i: Int): Int = if (i < 0) 0 else i
}
