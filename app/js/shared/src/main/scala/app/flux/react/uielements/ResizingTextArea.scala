package app.flux.react.uielements

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

object ResizingTextArea extends HydroReactComponent.Stateless {

  // **************** API ****************//
  def apply(tagMods: TagMod*): VdomElement = {
    component(Props(tagMods = tagMods.toVector))
  }

  // **************** Implementation of HydroReactComponent methods ****************//
  override protected val statelessConfig = StatelessComponentConfig(backendConstructor = new Backend(_))

  // **************** Implementation of HydroReactComponent types ****************//
  protected case class Props(tagMods: Seq[TagMod])

  protected class Backend($ : BackendScope[Props, State]) extends BackendBase($) with DidUpdate {
    val theInput = Ref[html.TextArea]

    override def render(props: Props, state: State): VdomElement = {
      <.textarea(
        ^.rows := 1,
      )(props.tagMods: _*).withRef(theInput)
    }

    override def didUpdate(prevProps: Props,
                           currentProps: Props,
                           prevState: State,
                           currentState: State): Callback = {
      theInput.get.map { input =>
        input.style.height = input.scrollHeight + "px"
      }
    }
  }
}
