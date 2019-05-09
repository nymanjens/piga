package app.flux.react.uielements

import hydro.flux.react.HydroReactComponent
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.MountedImpure
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.vdom.TagMod
import japgolly.scalajs.react.Ref.ToScalaComponent
import japgolly.scalajs.react.internal.Box
import org.scalajs.dom.html

import scala.collection.immutable.Seq
import scala.scalajs.js

object ResizingTextArea extends HydroReactComponent {

  // **************** API ****************//
  def apply(resizeStrategy: ResizeStrategy, ref: Reference)(tagMods: TagMod*): VdomElement = {
    ref.mutableRef.component(Props(resizeStrategy = resizeStrategy, tagMods = tagMods.toVector))
  }

  def ref(): Reference = new Reference(Ref.toScalaComponent(component))

  // **************** Implementation of HydroReactComponent methods ****************//
  override protected val config = ComponentConfig(backendConstructor = new Backend(_), initialState = State())

  // **************** Public inner types ****************//
  sealed trait ResizeStrategy
  case object ScaleWithInput extends ResizeStrategy
  case class Fixed(numberOfRows: Int) extends ResizeStrategy

  final class Reference private[ResizingTextArea] (private[ResizingTextArea] val mutableRef: ThisMutableRef) {
    def apply(): Proxy = new Proxy(mutableRef.get.asCallback.runNow())
  }

  final class Proxy private[ResizingTextArea] (maybeComponent: Option[ThisComponentU]) {
    def focus(): Unit = {
      for {
        component <- maybeComponent
        input <- component.backend.theInput.get.asCallback.runNow()
      } yield input.focus
    }
  }

  // **************** Implementation of HydroReactComponent types ****************//
  protected case class Props(resizeStrategy: ResizeStrategy, tagMods: Seq[TagMod])
  protected case class State(scrollHeight: Int = 20)

  private type ThisCtorSummoner = CtorType.Summoner.Aux[Box[Props], Children.None, CtorType.Props]
  private type ThisMutableRef = ToScalaComponent[Props, State, Backend, ThisCtorSummoner#CT]
  private type ThisComponentU = MountedImpure[Props, State, Backend]

  protected class Backend($ : BackendScope[Props, State]) extends BackendBase($) with DidUpdate {
    val theInput = Ref[html.TextArea]

    override def render(props: Props, state: State): VdomElement = {
      val textArea = props.resizeStrategy match {
        case ScaleWithInput =>
          <.textarea(
            ^.rows := 1,
            ^.style := js.Dictionary("height" -> s"${state.scrollHeight}px"),
          )
        case Fixed(numberOfRows) => <.textarea(^.rows := 1)
      }
      textArea(props.tagMods: _*).withRef(theInput)
    }

    override def didUpdate(prevProps: Props,
                           currentProps: Props,
                           prevState: State,
                           currentState: State): Callback = {
      theInput.get.flatMap { input =>
        if (currentState.scrollHeight == input.scrollHeight) {
          Callback.empty
        } else {
          $.modState(_.copy(scrollHeight = input.scrollHeight))
        }
      }
    }
  }
}
