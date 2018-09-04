package flux.react.uielements

import common.I18n
import common.LoggingUtils.LogExceptionsCallback
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js

final class WaitForFuture[V] {
  private val component = ScalaComponent
    .builder[Props](getClass.getSimpleName)
    .initialState(State(input = None))
    .renderPS((_, props, state) =>
      state.input match {
        case Some(input) => props.inputToElement(input)
        case None        => props.waitingElement
    })
    .componentWillMount($ =>
      LogExceptionsCallback {
        $.props.futureInput.map(input => $.modState(_.copy(input = Some(input))).runNow())
    })
    .build

  // **************** API ****************//
  def apply(futureInput: Future[V], waitingElement: VdomNode = null)(inputToElement: V => VdomNode)(
      implicit i18n: I18n): VdomElement = {
    component.apply(
      Props(
        futureInput = futureInput,
        inputToElement = inputToElement,
        waitingElement = Option(waitingElement) getOrElse defaultWaitingElement))
  }

  private def defaultWaitingElement(implicit i18n: I18n): VdomNode =
    <.div(^.style := js.Dictionary("padding" -> "200px 0  500px 60px"), s"${i18n("app.loading")}...")

  // **************** Private inner types ****************//
  private case class Props(futureInput: Future[V],
                           inputToElement: V => VdomNode,
                           waitingElement: VdomNode)
  private case class State(input: Option[V])
}
