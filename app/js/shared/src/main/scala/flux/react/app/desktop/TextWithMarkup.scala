package flux.react.app.desktop

import japgolly.scalajs.react.vdom.html_<^.{VdomNode, _}
import flux.react.app.desktop.TextWithMarkup.{Formatting, Part}
import japgolly.scalajs.react.vdom.VdomNode

import scala.collection.immutable.Seq

case class TextWithMarkup(parts: List[Part]) {

  lazy val contentString: String = parts.map(_.text).mkString

  lazy val toVdomNode: VdomNode = {
    def asVdomNode(x: VdomNode): VdomNode = x

    // TODO: addSpaceAfterTrailingNewline
    // TODO
    if (contentString.endsWith("\n")) {
      // Fix for tailing newline issue. The last \n is seemingly ignored unless a non-empty element is trailing
      contentString + ' '
    } else {
      contentString
    }
  }

  def +(that: TextWithMarkup): TextWithMarkup = TextWithMarkup(this.parts ++ that.parts)

  def formattingAtCursor(offset: Int): Formatting = {
    // TODO: empty part --> includde
    // TODO: in link and next cursor not in link --> no link
    ???
  }

  def sub(beginOffset: Int, endOffset: Int = -1): TextWithMarkup = {
    def subInner(parts: List[Part], beginOffset: Int, endOffset: Int): List[Part] = parts match {
      case Nil => Nil
      case part :: rest if beginOffset < part.text.length && endOffset <= part.text.length =>
        part.sub(beginOffset, endOffset) :: Nil
      case part :: rest if beginOffset < part.text.length =>
        part.sub(beginOffset) :: subInner(rest, beginOffset = 0, endOffset - part.text.length)
      case part :: rest =>
        subInner(rest, beginOffset - part.text.length, endOffset - part.text.length)
    }
    TextWithMarkup(
      subInner(parts, beginOffset, endOffset = if (endOffset == -1) contentString.length else endOffset))
  }
}

object TextWithMarkup {

  val empty: TextWithMarkup = TextWithMarkup(Nil)

  def canonicalize(text: TextWithMarkup): TextWithMarkup = text // TODO: Implement

  case class Part(text: String, formatting: Formatting = Formatting.none) {
    private[TextWithMarkup] def sub(beginOffset: Int, endOffset: Int = -1): Part =
      copy(
        text = if (endOffset == -1) text.substring(beginOffset) else text.substring(beginOffset, endOffset))
  }
  case class Formatting(bold: Boolean = false,
                        italic: Boolean = false,
                        code: Boolean = false,
                        link: Option[String] = None)
  object Formatting {
    val none = Formatting()
  }
}
