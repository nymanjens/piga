package app.flux.react.app.document

import app.flux.react.uielements.ResizingTextArea
import app.flux.react.uielements.ResizingTextArea.Fixed
import app.flux.react.uielements.ResizingTextArea.ScaleWithInput
import hydro.flux.react.ReactVdomUtils.<<
import hydro.flux.react.ReactVdomUtils.^^
import app.flux.stores.document.DocumentStore
import app.models.document.Document
import app.models.document.Task
import hydro.common.I18n
import hydro.common.ScalaUtils.ifThenOption
import hydro.common.Tags
import hydro.common.time.Clock
import hydro.flux.react.HydroReactComponent
import hydro.flux.react.ReactVdomUtils.^^
import hydro.flux.react.uielements.Bootstrap
import hydro.flux.react.uielements.Bootstrap.Size
import hydro.flux.react.uielements.Bootstrap.Variant
import hydro.flux.router.RouterContext
import hydro.models.access.EntityAccess
import japgolly.scalajs.react._
import japgolly.scalajs.react.raw.SyntheticEvent
import japgolly.scalajs.react.vdom.html_<^._

import scala.collection.immutable.Seq
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
  }

  protected class Backend($ : BackendScope[Props, State]) extends BackendBase($) {

    private val editHistory: EditHistory = new EditHistory()

    override def render(props: Props, state: State): VdomElement = {
      implicit val router = props.router
      <.span(
        ^.className := "mobile-task-editor",
        <.ul(
          applyCollapsedProperty(state.document.tasks).map {
            case (task, taskIndex, maybeAmountCollapsed) =>
              val nodeType = state.document.tasksOption(taskIndex + 1) match {
                case _ if task.indentation == 0                                => "root"
                case Some(nextTask) if nextTask.indentation > task.indentation => "node"
                case _                                                         => "leaf"
              }
              val isReadOnly = !task.content.isPlainText || task.content.containsLink
              <.li(
                ^.key := s"li-${task.id}",
                ^.style := js.Dictionary("marginLeft" -> s"${task.indentation * 20}px"),
                ^^.classes(
                  Seq(nodeType) ++
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
                  else plainTextInput(task, taskIsHighlighted = state.highlightedTaskIndex == taskIndex)
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
        editButtons()
      )
    }

    private def plainTextInput(task: Task, taskIsHighlighted: Boolean): VdomNode = {
      ResizingTextArea(
        resizeStrategy = if (taskIsHighlighted) ScaleWithInput else Fixed(numberOfRows = 1),
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

    private def editButtons(): VdomNode = <.div(
      ^.className := "edit-buttons",
      Bootstrap.Button(Variant.info, Size.sm)(
        Bootstrap.FontAwesomeIcon("dedent", fixedWidth = true),
      ),
      Bootstrap.Button(Variant.info, Size.sm)(
        Bootstrap.FontAwesomeIcon("indent", fixedWidth = true),
      ),
      Bootstrap.Button(Variant.info, Size.sm)(
        Bootstrap.FontAwesomeIcon("chevron-up", fixedWidth = true),
      ),
      Bootstrap.Button(Variant.info, Size.sm)(
        Bootstrap.FontAwesomeIcon("chevron-down", fixedWidth = true),
      ),
      Bootstrap.Button(Variant.info, Size.sm)(
        Bootstrap.FontAwesomeIcon("trash-o", fixedWidth = true),
      ),
      Bootstrap.Button(Variant.info, Size.sm)(
        Bootstrap.FontAwesomeIcon("calendar-o", fixedWidth = true),
      ),
      Bootstrap.Button(Variant.info, Size.sm)(
        Bootstrap.FontAwesomeIcon("rotate-left", fixedWidth = true),
      ),
      Bootstrap.Button(Variant.info, Size.sm)(
        Bootstrap.FontAwesomeIcon("rotate-right", fixedWidth = true),
      ),
    )

    private def onPlainTextChange(newContent: String, originalTask: Task): Callback = {
      println("onPlainTextChange()")
      Callback.empty
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

    //
    //    private def removeTasks(taskIndices: Range)(implicit state: State, props: Props): Callback = {
    //      implicit val oldDocument = state.document
    //
    //      val removedTasks = for (i <- taskIndices) yield oldDocument.tasks(i)
    //      val addedTasks =
    //        if (oldDocument.tasks.size > taskIndices.size) Seq()
    //        else // Removing all tasks in this document --> Replace the last task with an empty task
    //          Seq(
    //            Task.withRandomId(
    //              content = TextWithMarkup.empty,
    //              orderToken = OrderToken.middle,
    //              indentation = 0,
    //              collapsed = false,
    //              delayedUntil = None,
    //              tags = Seq()
    //            ))
    //
    //      replaceWithHistory(
    //        edit = DocumentEdit.Reversible(removedTasks = removedTasks, addedTasks = addedTasks),
    //        selectionBeforeEdit = IndexedSelection(
    //          IndexedCursor.atStartOfTask(taskIndices.head),
    //          IndexedCursor.atEndOfTask(taskIndices.last)),
    //        selectionAfterEdit = IndexedSelection.singleton(
    //          IndexedCursor.atStartOfTask(
    //            if (oldDocument.tasks.size > taskIndices.head + taskIndices.size) taskIndices.head
    //            else if (taskIndices.head == 0) 0
    //            else taskIndices.head - 1))
    //      )
    //    }

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
  }
}
