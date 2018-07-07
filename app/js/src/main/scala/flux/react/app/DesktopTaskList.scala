package flux.react.app

import common.I18n
import common.LoggingUtils.{logExceptions, LogExceptionsCallback}
import flux.react.router.RouterContext
import flux.react.uielements
import flux.react.uielements.Panel
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import models.access.EntityAccess
import models.user.User

private[app] final class DesktopTaskList(implicit user: User, entityAccess: EntityAccess, i18n: I18n) {

  private val component = ScalaComponent
    .builder[Props](getClass.getSimpleName)
    .initialState(State())
    .renderBackend[Backend]
    .build

  // **************** API ****************//
  def apply(router: RouterContext): VdomElement = {
    component(Props(router))
  }

  // **************** Private inner types ****************//
  private case class Props(router: RouterContext)
  private case class State(content: VdomElement = <.span("Hello ", <.b("world")))

  private class Backend($ : BackendScope[Props, State]) {
    def render(props: Props, state: State): VdomElement = logExceptions {
      implicit val router = props.router
      <.span(
        uielements.PageHeader(router.currentPage),
        Panel(
          title = "Piga Task List"
        ) {
          <.div(
            ^.contentEditable := true,
            ^.onInput ==> ((event: ReactEventFromInput) => onChange(event.target.value)),
            ^.onBlur ==> ((event: ReactEventFromInput) => onChange(event.target.value)),
            state.content)
        }
      )
    }

    private def onChange(value: String): Callback = LogExceptionsCallback {
      println(value)
    }
  }
}
