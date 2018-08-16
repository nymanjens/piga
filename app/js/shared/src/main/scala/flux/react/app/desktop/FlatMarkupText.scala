package flux.react.app.desktop

import japgolly.scalajs.react.vdom.html_<^.{VdomNode, _}
import flux.react.app.desktop.FlatMarkupText.Formatting
import japgolly.scalajs.react.vdom.VdomNode

import scala.collection.immutable.Seq

case class FlatMarkupText(parts: Seq[FlatMarkupText.Part]) {

  lazy val contentString: String = ???

  lazy val toVdomNode: VdomNode = {
    def asVdomNode(x: VdomNode): VdomNode = x

    // TODO: addSpaceAfterTrailingNewline
    ???
  }

  def +(other: FlatMarkupText): FlatMarkupText = ???

  def formattingAtCursor(offset: Int): Formatting = ???

  def sub(beginOffset: Int, endOffset: Int = -1): FlatMarkupText = ???
}

object FlatMarkupText {

  val empty: FlatMarkupText = FlatMarkupText(Seq())

  def canonicalize(text: FlatMarkupText): FlatMarkupText = text // TODO: Implement

  case class Part(text: String, formatting: Formatting = Formatting.none)
  case class Formatting(bold: Boolean = false,
                        italic: Boolean = false,
                        code: Boolean = false,
                        link: Option[String] = None)
  object Formatting {
    val none = Formatting()
  }
}
