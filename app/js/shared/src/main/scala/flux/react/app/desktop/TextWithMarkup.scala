package flux.react.app.desktop

import common.LoggingUtils
import japgolly.scalajs.react.vdom.html_<^.{VdomNode, _}
import flux.react.app.desktop.TextWithMarkup.{Formatting, Part}
import japgolly.scalajs.react.vdom.{VdomArray, VdomNode}

import scala.collection.immutable.Seq
import scala.collection.mutable

case class TextWithMarkup(parts: List[Part]) {

  lazy val contentString: String = parts.map(_.text).mkString

  lazy val toVdomNode: VdomNode = {
    trait FormattingOption[T] {
      def getValue(part: Part): T
      def apply(value: T, children: VdomNode): VdomNode
    }
    object Bold extends FormattingOption[Boolean] {
      override def getValue(part: Part): Boolean = part.formatting.bold
      override def apply(value: Boolean, children: VdomNode): VdomNode =
        if (value) <.b(children) else children
    }
    object Italic extends FormattingOption[Boolean] {
      override def getValue(part: Part): Boolean = part.formatting.italic
      override def apply(value: Boolean, children: VdomNode): VdomNode =
        if (value) <.i(children) else children
    }
    object Code extends FormattingOption[Boolean] {
      override def getValue(part: Part): Boolean = part.formatting.code
      override def apply(value: Boolean, children: VdomNode): VdomNode =
        if (value) <.code(children) else children
    }
    object Link extends FormattingOption[Option[String]] {
      override def getValue(part: Part): Option[String] = part.formatting.link
      override def apply(value: Option[String], children: VdomNode): VdomNode =
        if (value.isDefined) <.a(^.href := value.get, children) else children
    }

    def addSpaceAfterTrailingNewline(parts: List[Part]): List[Part] = {
      if (parts.nonEmpty && parts.last.text.endsWith("\n")) {
        // Fix for tailing newline issue. The last \n is seemingly ignored unless a non-empty element is trailing
        parts.updated(parts.length - 1, parts.last + " ")
      } else {
        parts
      }
    }
    def toVdomNodeInner(parts: Seq[Part], formattingLeft: List[FormattingOption[_]]): VdomNode = {
      formattingLeft match {
        case Nil => parts.map(_.text).mkString
        case formattingOption :: otherFormattingOptions =>
          def inner[T](formattingOption: FormattingOption[T]): VdomNode = {
            var currentFormattingValue: T = null.asInstanceOf[T]
            val partBuffer = mutable.Buffer[Part]()
            val resultBuffer = mutable.Buffer[VdomNode]()

            def pushToBuffer(): Unit = {
              if (partBuffer.nonEmpty) {
                resultBuffer.append(formattingOption
                  .apply(currentFormattingValue, toVdomNodeInner(partBuffer.toList, otherFormattingOptions)))
                partBuffer.clear()
              }
            }

            for (part <- parts) {
              formattingOption.getValue(part) match {
                case value if value == currentFormattingValue =>
                  partBuffer.append(part)
                case newFormattingValue =>
                  pushToBuffer()
                  currentFormattingValue = newFormattingValue
              }
              pushToBuffer()
            }
            resultBuffer.toVdomArray
          }
          inner(formattingOption)
      }
    }

    toVdomNodeInner(addSpaceAfterTrailingNewline(parts), formattingLeft = List(Link, Code, Italic, Bold))
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

    private[TextWithMarkup] def +(thatText: String): Part = copy(text = this.text + thatText)
  }

  case class Formatting(bold: Boolean = false,
                        italic: Boolean = false,
                        code: Boolean = false,
                        link: Option[String] = None)
  object Formatting {
    val none = Formatting()
  }
}
