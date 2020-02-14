package hydro.flux.react.uielements.dbexplorer

import hydro.common.I18n
import hydro.flux.react.ReactVdomUtils.<<
import hydro.flux.react.ReactVdomUtils.^^
import hydro.common.JsLoggingUtils.logExceptions
import hydro.flux.react.HydroReactComponent
import hydro.flux.react.uielements.Table
import hydro.flux.react.uielements.Table.TableRowData
import hydro.flux.stores.DatabaseExplorerStoreFactory
import hydro.models.access.JsEntityAccess
import hydro.models.modification.EntityType
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

import scala.collection.immutable.Seq

private[dbexplorer] final class DatabaseTableView(
    implicit jsEntityAccess: JsEntityAccess,
    i18n: I18n,
    databaseExplorerStoreFactory: DatabaseExplorerStoreFactory,
) extends HydroReactComponent {

  // **************** API ****************//
  def apply(entityType: EntityType.any): VdomElement = {
    component(Props(entityType = entityType))
  }

  // **************** Implementation of HydroReactComponent methods ****************//
  override protected val config = ComponentConfig(backendConstructor = new Backend(_), initialState = State())
    .withStateStoresDependencyFromProps { props =>
      val store = databaseExplorerStoreFactory.get(props.entityType)
      StateStoresDependency(store, _.copy(maybeStoreState = store.state))
    }

  // **************** Implementation of HydroReactComponent types ****************//
  protected case class Props(entityType: EntityType.any)
  protected case class State(
      expanded: Boolean = false,
      maybeStoreState: Option[DatabaseExplorerStoreFactory.State] = None,
  )

  protected final class Backend(val $ : BackendScope[Props, State]) extends BackendBase($) {

    override def render(props: Props, state: State) = logExceptions {
      Table(
        title = props.entityType.entityClass.getSimpleName,
        tableClasses = Seq(),
        expanded = state.expanded,
        onToggleCollapsedExpanded = Some(() => $.modState(s => s.copy(expanded = !s.expanded)).runNow()),
        tableTitleExtra = <<.ifDefined(state.maybeStoreState) { storeState =>
          s"(${storeState.allEntities.size} entries)"
        },
        tableHeaders = tableHeaders(),
        tableRowDatas = tableRowDatas(),
      )
    }

    private def tableHeaders(): Seq[VdomNode] = Seq()
    private def tableRowDatas(): Seq[TableRowData] = Seq()
  }
}
