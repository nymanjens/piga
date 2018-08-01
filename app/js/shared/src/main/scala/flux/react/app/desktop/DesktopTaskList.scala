package flux.react.app.desktop

import common.I18n
import common.LoggingUtils.logExceptions
import flux.react.router.RouterContext
import flux.react.uielements
import flux.react.uielements.Panel
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import jsfacades.ReactContentEditable
import models.access.EntityAccess
import models.user.User
import org.scalajs.dom.console
import org.scalajs.dom.raw.KeyboardEvent

import scala.scalajs.js

private[app] final class DesktopTaskList(implicit entityAccess: EntityAccess,
                                         i18n: I18n,
                                         taskEditor: TaskEditor) {

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
//  private case class State(content: VdomElement = <.span("Hello ", <.b("world")))
  private case class State(content: String = "Hello <b>World</b>!",
                           lines: Seq[String] = Seq("Hello", "World!"))

  private class Backend($ : BackendScope[Props, State]) {
    def render(props: Props, state: State): VdomElement = logExceptions {
      implicit val router = props.router
      <.span(
        uielements.PageHeader(router.currentPage),
        Panel(
          title = "Piga Task List"
        ) {
//          <.span(
//            (for ((line, i) <- state.lines.zipWithIndex)
//              yield <.div(^.key := s"line-$i", "- ", line)).toVdomArray,
//            <.br(),
//            "----------------",
//            <.br(),
//            <.br(),
//            <.br(),
//            ReactContentEditable(toContent(state.lines), onChange = onChange, onKeyDown = handleKeyDown)
//          )
          <.span(taskEditor())
        }
      )
    }

    private def onChange(content: String): Unit = {
      def sanitize(s: String): String = s.replaceAll("\\<.*?\\>", "")
      val lines = for {
        l1 <- content.split("<br>")
        l2 <- l1.split("</li><li>")
        l3 <- Seq(sanitize(l2))
        if l3.nonEmpty
      } yield l3

      $.modState(_.copy(lines = lines)).runNow()
    }

    def handleKeyDown(event: KeyboardEvent): Unit = {
      console.log(event)
      console.log(event.asInstanceOf[js.Dynamic].target.selectionStart)
      event.preventDefault()
    }

    private def toContent(lines: Seq[String]): String = {
      s"<ul><li>${lines.mkString("</li><li>")}</li></ul>"
    }
  }
}
