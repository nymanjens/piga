package app.flux.react.app.document

import app.flux.react.app.document.TaskEditorUtils.TaskInSeq
import app.flux.react.app.document.TaskEditorUtils.applyCollapsedProperty
import app.flux.react.uielements.ResizingTextArea
import app.flux.react.uielements.ResizingTextArea.Fixed
import app.flux.react.uielements.ResizingTextArea.ScaleWithInput
import app.flux.stores.document.DocumentStore
import app.models.document.Document
import app.models.document.Document.IndexedCursor
import app.models.document.Document.IndexedSelection
import app.models.document.DocumentEdit
import app.models.document.DocumentEdit.MaskedTaskUpdate
import app.models.document.Task
import app.models.document.TextWithMarkup
import hydro.common.I18n
import hydro.common.ScalaUtils.ifThenOption
import hydro.common.Tags
import hydro.common.time.Clock
import hydro.common.CollectionUtils
import hydro.common.GuavaReplacement
import hydro.common.GuavaReplacement.LoadingCache
import hydro.common.OrderToken
import hydro.common.ScalaUtils.visibleForTesting
import hydro.flux.react.HydroReactComponent
import hydro.flux.react.ReactVdomUtils.<<
import hydro.flux.react.ReactVdomUtils.^^
import hydro.flux.react.uielements.Bootstrap
import hydro.flux.react.uielements.Bootstrap.Size
import hydro.flux.react.uielements.Bootstrap.Variant
import hydro.flux.router.RouterContext
import hydro.models.access.EntityAccess
import japgolly.scalajs.react._
import japgolly.scalajs.react.raw.SyntheticEvent
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom

import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.scalajs.js

private[document] final class MobileTaskEditor(implicit entityAccess: EntityAccess, i18n: I18n, clock: Clock)
    extends HydroReactComponent {

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
  protected case class State(document: Document = Document.nullInstance,
                             pendingTaskIds: Set[Long] = Set(),
                             highlightedTaskIndex: Int = 0,
  ) {
    def copyFromStore(documentStore: DocumentStore): State =
      copy(document = documentStore.state.document, pendingTaskIds = documentStore.state.pendingTaskIds)

    lazy val highlightedTask: Task = document.tasks(highlightedTaskIndex)
    def startOfHighlightedTask: IndexedSelection =
      IndexedSelection.singleton(IndexedCursor.atStartOfTask(highlightedTaskIndex))

  }

  protected class Backend($ : BackendScope[Props, State]) extends BackendBase($) {

    private val taskIdToInputRef: LoadingCache[Long, ResizingTextArea.Reference] =
      LoadingCache.fromLoader(_ => ResizingTextArea.ref())
    private val editHistory: EditHistory = new EditHistory()

    override def render(props: Props, state: State): VdomElement = {
      implicit val router = props.router
      implicit val implicitState = state
      implicit val implicitProps = props
      <.span(
        ^.className := "mobile-task-editor",
        <.ul(
          applyCollapsedProperty(state.document.tasks).map {
            case TaskInSeq(task, taskIndex, maybeAmountCollapsed, isRoot, isLeaf) =>
              val isReadOnly = !task.content.isPlainText || task.content.containsLink

              <.li(
                ^.key := s"li-${task.id}",
                ^.style := js.Dictionary("marginLeft" -> s"${task.indentation * 20}px"),
                ^^.classes(
                  Seq() ++
                    ifThenOption(isRoot)("root") ++
                    ifThenOption(isLeaf)("leaf") ++
                    ifThenOption(task.contentString.isEmpty)("empty-task") ++
                    ifThenOption(task.collapsed)("collapsed") ++
                    ifThenOption(state.highlightedTaskIndex == taskIndex)("highlighted") ++
                    ifThenOption(state.pendingTaskIds contains task.id)("modification-pending") ++
                    ifThenOption(isReadOnly)("read-only")),
                ^.onClick --> selectTask(task),
                task.tags.zipWithIndex.map {
                  case (tag, tagIndex) =>
                    <.div( // This is a holder for the label to avoid tags to be affected by the surrounding flex box
                      ^.key := tagIndex,
                      ^.className := "tag-holder",
                      <.span(
                        ^^.classes("tag", "label", s"label-${Tags.getBootstrapClassSuffix(tag)}"),
                        tag,
                      )
                    )
                }.toVdomArray, {
                  if (isReadOnly) readonlyTask(task)
                  else plainTextInput(task)
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
        ^.value := task.contentString,
        ^.spellCheck := false,
        ^.onFocus --> selectTask(task),
        ^.onChange ==> { (event: ReactEventFromInput) =>
          onPlainTextChange(newContent = event.target.value, originalTask = task)
        },
      )
    }

    private def readonlyTask(task: Task): VdomNode = {
      <.span(
        ^.className := "readonly-task",
        task.content.toVdomNode,
      )
    }

    private def editButtons(implicit props: Props, state: State): VdomNode = <.div(
      ^.className := "edit-buttons",
      Bootstrap.ButtonGroup(
        // Create empty
        Bootstrap.Button(Variant.info, Size.sm)(
          ^.onClick --> creatEmptyTaskUnderHighlighted(),
          Bootstrap.FontAwesomeIcon("calendar-o", fixedWidth = true),
        ),
        // Delete
        Bootstrap.Button(Variant.info, Size.sm)(
          ^.onClick --> removeHighlightedTask(),
          ^.disabled := state.document.tasks.size == 1,
          Bootstrap.FontAwesomeIcon("trash-o", fixedWidth = true),
        ),
      ),
      Bootstrap.ButtonGroup(
        // Move up
        Bootstrap.Button(Variant.info, Size.sm)(
          ^.onClick --> moveHighlightedTask(direction = -1),
          ^.disabled := state.highlightedTaskIndex == 0,
          Bootstrap.FontAwesomeIcon("chevron-up", fixedWidth = true),
        ),
        // Move down
        Bootstrap.Button(Variant.info, Size.sm)(
          ^.onClick --> moveHighlightedTask(direction = +1),
          ^.disabled := state.highlightedTaskIndex == state.document.tasks.size - 1,
          Bootstrap.FontAwesomeIcon("chevron-down", fixedWidth = true),
        ),
        // Dedent
        Bootstrap.Button(Variant.info, Size.sm)(
          ^.onClick --> indentHighlightedTask(indentIncrease = -1),
          ^.disabled := state.highlightedTask.indentation == 0,
          Bootstrap.FontAwesomeIcon("dedent", fixedWidth = true),
        ),
        // Indent
        Bootstrap.Button(Variant.info, Size.sm)(
          ^.onClick --> indentHighlightedTask(indentIncrease = +1),
          Bootstrap.FontAwesomeIcon("indent", fixedWidth = true),
        ),
      ),
      Bootstrap.ButtonGroup(
        // Undo
        Bootstrap.Button(Variant.info, Size.sm)(
          ^.disabled := !editHistory.canUndo,
          Bootstrap.FontAwesomeIcon("rotate-left", fixedWidth = true),
        ),
        // Redo
        Bootstrap.Button(Variant.info, Size.sm)(
          ^.disabled := !editHistory.canRedo,
          Bootstrap.FontAwesomeIcon("rotate-right", fixedWidth = true),
        ),
      ),
    )

    private def onPlainTextChange(newContent: String, originalTask: Task)(implicit state: State,
                                                                          props: Props): Callback = {
      replaceWithHistory(
        edit = DocumentEdit.Reversible(
          taskUpdates = Seq(
            MaskedTaskUpdate.fromFields(originalTask = originalTask, content = TextWithMarkup(newContent)))),
        replacementString =
          deriveReplacementString(oldContent = originalTask.contentString, newContent = newContent),
      )
    }

    private def selectTask(task: Task): Callback = {
      $.modState { state =>
        state.document.maybeIndexOf(task.id, orderTokenHint = task.orderToken) match {
          case None            => state
          case Some(taskIndex) => state.copy(highlightedTaskIndex = taskIndex)
        }
      }
    }

    private def preventDefault(event: SyntheticEvent[_]): Callback = {
      event.preventDefault()
      Callback.empty
    }

    private def creatEmptyTaskUnderHighlighted()(implicit state: State, props: Props): Callback = {
      implicit val oldDocument = state.document

      val insertIndex =
        IndexedSelection
          .atStartOfTask(state.highlightedTaskIndex)
          .includeChildren(collapsedOnly = true)
          .end
          .seqIndex

      replaceWithHistory(
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
    }

    private def removeHighlightedTask()(implicit state: State, props: Props): Callback = {
      implicit val oldDocument = state.document

      val indicesToRemove =
        IndexedSelection.atStartOfTask(state.highlightedTaskIndex).includeChildren().seqIndices
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
        replacementString: String = "")(implicit oldState: State, props: Props): Callback = {

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
        }
      )
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
}
