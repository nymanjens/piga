package hydro.flux.react.uielements.dbexplorer

import hydro.common.JsLoggingUtils.logExceptions
import hydro.flux.react.HydroReactComponent
import hydro.models.access.JsEntityAccess
import hydro.models.modification.EntityType
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

private[dbexplorer] final class DatabaseTableView(
    implicit jsEntityAccess: JsEntityAccess,
) extends HydroReactComponent {

  // **************** API ****************//
  def apply(entityType: EntityType.any): VdomElement = {
    component(Props(entityType = entityType))
  }

  // **************** Implementation of HydroReactComponent methods ****************//
  override protected val config = ComponentConfig(backendConstructor = new Backend(_), initialState = State())

  // **************** Implementation of HydroReactComponent types ****************//
  protected case class Props(entityType: EntityType.any)
  protected case class State(expanded: Boolean = false)

  protected final class Backend(val $ : BackendScope[Props, State]) extends BackendBase($) {

    override def render(props: Props, state: State) = logExceptions {
      <.h3(props.entityType.entityClass.getSimpleName)
      // Maybe use Table here
    }
  }
}
