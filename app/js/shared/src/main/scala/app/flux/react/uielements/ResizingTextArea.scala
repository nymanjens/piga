package app.flux.react.uielements

import app.models.document.DocumentEntity
import hydro.flux.react.uielements.SbadminLayout
import hydro.flux.react.HydroReactComponent
import hydro.flux.router.RouterContext
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.vdom.TagMod

import scala.collection.immutable.Seq

object ResizingTextArea extends HydroReactComponent.Stateless {

  // **************** API ****************//
  def apply(tagMods: TagMod*): VdomElement = {
    component(Props(tagMods = tagMods.toVector))
  }

  // **************** Implementation of HydroReactComponent methods ****************//
  override protected val statelessConfig = StatelessComponentConfig(backendConstructor = new Backend(_))

  // **************** Implementation of HydroReactComponent types ****************//
  protected case class Props(tagMods: Seq[TagMod])

  protected class Backend($ : BackendScope[Props, State]) extends BackendBase($) {
    override def render(props: Props, state: State): VdomElement = {
      <.textarea(props.tagMods: _*)
    }
  }
}
