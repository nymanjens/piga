package jsfacades

import common.LoggingUtils
import common.LoggingUtils.logExceptions
import japgolly.scalajs.react.{Children, JsComponent}
import org.scalajs.dom

import scala.scalajs.js
import scala2js.Converters._

import scala.scalajs.js.annotation.JSImport

object ReactContentEditable {

  // **************** API ****************//
  def apply(html: String, onChange: String => Unit) = {
    val component = JsComponent[js.Object, Children.None, Null](RawComponent)
    component(
      Props(
        html = html,
        onChange = evt =>
          logExceptions {
            onChange(evt.target.value.asInstanceOf[String])
        }).toJsObject)
  }

  // **************** Private inner types ****************//
  @JSImport("react-contenteditable", JSImport.Namespace)
  @js.native
  private object RawComponent extends js.Object

  private case class Props(html: String, onChange: js.Function1[js.Dynamic, Unit]) {
    def toJsObject: js.Object =
      js.Dynamic.literal(
        html = html,
        disabled = false,
        onChange = onChange
      )
  }
}
