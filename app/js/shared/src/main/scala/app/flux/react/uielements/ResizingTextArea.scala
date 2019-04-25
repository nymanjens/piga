package app.flux.react.uielements

import hydro.flux.react.ReactVdomUtils.<<
import hydro.flux.react.ReactVdomUtils.^^
import app.models.document.DocumentEntity
import hydro.flux.react.uielements.SbadminLayout
import hydro.flux.react.HydroReactComponent
import hydro.flux.router.RouterContext
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.vdom.TagMod
import org.scalajs.dom.console
import org.scalajs.dom.html
import org.scalajs.dom.html.Input

import scala.collection.immutable.Seq
import scala.scalajs.js

object ResizingTextArea extends HydroReactComponent {

  // **************** API ****************//
  def apply(resizeStrategy: ResizeStrategy)(tagMods: TagMod*): VdomElement = {
    component(Props(resizeStrategy = resizeStrategy, tagMods = tagMods.toVector))
  }

  // **************** Implementation of HydroReactComponent methods ****************//
  override protected val config = ComponentConfig(backendConstructor = new Backend(_), initialState = State())

  // **************** Public inner types ****************//
  sealed trait ResizeStrategy
  case object ScaleWithInput extends ResizeStrategy
  case class Fixed(numberOfRows: Int) extends ResizeStrategy

  // **************** Implementation of HydroReactComponent types ****************//
  protected case class Props(resizeStrategy: ResizeStrategy, tagMods: Seq[TagMod])
  protected case class State(scrollHeight: Int = 20)

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
