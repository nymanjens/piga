package jsfacades

import japgolly.scalajs.react.{Children, JsComponent}

import scala.scalajs.js
import scala2js.Converters._

object ReactContentEditable {

  // **************** API ****************//
  def apply(html: String, onChange: String => Unit) = {
    val component = JsComponent[js.Object, Children.None, Null](js.Dynamic.global.ContentEditable)
    component(
      Props(html = html, onChange = evt => onChange(evt.target.value.asInstanceOf[String])).toJsObject)
  }

  // **************** Private inner types ****************//
  private case class Props(html: String, onChange: js.Function1[js.Dynamic, Unit]) {
    def toJsObject: js.Object =
      js.Dynamic.literal(
        html = html,
        onChange = onChange
      )
  }
}
