package flux.react.uielements

import common.I18n
import flux.react.router.Page
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

object PageHeader {
  private val component = ScalaComponent
    .builder[Props](getClass.getSimpleName)
    .renderPC { (_, props, children) =>
      <.h1(
        ^.className := "page-header",
        <.i(^.className := props.iconClass),
        " ",
        props.title,
      )
    }
    .build

  // **************** API ****************//
  def apply(page: Page, title: String = null)(implicit i18n: I18n): VdomElement = {
    component(Props(title = Option(title) getOrElse page.title(i18n), iconClass = page.iconClass))()
  }

  // **************** Private inner types ****************//
  private case class Props(title: String, iconClass: String)(implicit val i18n: I18n)
}
