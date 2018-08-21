package flux.react.app.desktop

import common.DomNodeUtils._
import common.GuavaReplacement.Splitter
import org.scalajs.dom
import common.LoggingUtils
import japgolly.scalajs.react.vdom.html_<^.{VdomNode, _}
import flux.react.app.desktop.TextWithMarkup.{Formatting, FormattingOption, Part}
import japgolly.scalajs.react.vdom.{VdomArray, VdomNode}
import jsfacades.escapeHtml

import scala.collection.immutable.Seq
import scala.collection.mutable

case class TextWithMarkup(parts: List[Part]) {

  lazy val contentString: String = parts.map(_.text).mkString

  lazy val toVdomNode: VdomNode = {
    def addSpaceAfterTrailingNewline(parts: List[Part]): List[Part] = {
      if (parts.nonEmpty && parts.last.text.endsWith("\n")) {
        // Fix for tailing newline issue. The last \n is seemingly ignored unless a non-empty element is trailing
        parts.updated(parts.length - 1, parts.last + " ")
      } else {
        parts
      }
    }

    val applyFormattingOption = new FormattingOption.Applier[VdomNode] {
      private var keyCounter = 0

      override def apply[T](option: FormattingOption[T],
                            value: T,
                            children: VdomNode,
                            childrenParts: Iterable[Part]): VdomNode = {
        def asBoolean(t: T): Boolean = t.asInstanceOf[Boolean]
        def asOption(t: T): Option[String] = t.asInstanceOf[Option[String]]
        def key: String = {
          keyCounter += 1
          keyCounter + "_" + childrenParts.map(_.text).mkString
        }

        option match {
          case FormattingOption.Bold   => if (asBoolean(value)) <.b(^.key := key, children) else children
          case FormattingOption.Italic => if (asBoolean(value)) <.i(^.key := key, children) else children
          case FormattingOption.Code   => if (asBoolean(value)) <.code(^.key := key, children) else children
          case FormattingOption.Link =>
            if (asOption(value).isDefined) {
              <.a(^.href := asOption(value).get, ^.target := "blank", ^.key := key, children)
            } else children
        }
      }
    }

    TextWithMarkup.serializeToDom[VdomNode](
      addSpaceAfterTrailingNewline(parts),
      applyFormattingOption = applyFormattingOption,
      liftString = s => s,
      mergeResults = _.toVdomArray)
  }

  def toHtml: String = {
    val applyFormattingOption = new FormattingOption.Applier[String] {
      override def apply[T](option: FormattingOption[T],
                            value: T,
                            children: String,
                            childrenParts: Iterable[Part]): String = {
        def asBoolean(t: T): Boolean = t.asInstanceOf[Boolean]
        def asOption(t: T): Option[String] = t.asInstanceOf[Option[String]]

        option match {
          case FormattingOption.Bold   => if (asBoolean(value)) s"<b>$children</b>" else children
          case FormattingOption.Italic => if (asBoolean(value)) s"<i>$children</i>" else children
          case FormattingOption.Code   => if (asBoolean(value)) s"<code>$children</code>" else children
          case FormattingOption.Link =>
            if (asOption(value).isDefined) s"""<a href="${asOption(value).get}">$children</a>""" else children
        }
      }
    }

    TextWithMarkup.serializeToDom[String](
      parts,
      applyFormattingOption = applyFormattingOption,
      liftString = s => escapeHtml(s).replace("\n", "<br />"),
      mergeResults = _.mkString)
  }

  def +(that: TextWithMarkup): TextWithMarkup = TextWithMarkup(this.parts ++ that.parts)

  def formattingAtCursor(offset: Int): Formatting = {
    def formattingAtCursorInner(parts: List[Part], offset: Int): Formatting =
      parts match {
        case Nil => Formatting.none
        case part :: rest if offset <= part.text.length =>
          if (part.formatting.link.nonEmpty && offset == part.text.length) {
            val nextFormatting = formattingAtCursorInner(rest, offset = 0)
            // If in link and next cursor not in link --> no link
            if (part.formatting.link != nextFormatting.link) {
              part.formatting.copy(link = None)
            } else {
              part.formatting
            }
          } else {
            part.formatting
          }
        case part :: rest =>
          formattingAtCursorInner(rest, offset - part.text.length)
      }
    formattingAtCursorInner(parts, offset)
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

    if (beginOffset == endOffset) {
      TextWithMarkup.empty
    } else {
      TextWithMarkup(
        subInner(parts, beginOffset, endOffset = if (endOffset == -1) contentString.length else endOffset))
    }
  }

  def withFormatting(beginOffset: Int,
                     endOffset: Int,
                     updateFunc: Formatting => Formatting): TextWithMarkup = {
    def updated(textWithMarkup: TextWithMarkup): TextWithMarkup = {
      TextWithMarkup(textWithMarkup.parts.map(part => part.copy(formatting = updateFunc(part.formatting))))
    }
    sub(0, beginOffset) + updated(sub(beginOffset, endOffset)) + sub(endOffset, contentString.length)
  }

  def canonicalized: TextWithMarkup = TextWithMarkup {
    def canonicalizedInner(parts: List[Part]): List[Part] = parts match {
      case part1 :: part2 :: rest if part1.formatting == part2.formatting =>
        canonicalizedInner(Part(part1.text + part2.text, part1.formatting) :: rest)
      case Nil          => Nil
      case part :: rest => part :: canonicalizedInner(rest)
    }
    canonicalizedInner(parts)
  }
}

object TextWithMarkup {

  val empty: TextWithMarkup = TextWithMarkup(Nil)

  def withoutFormatting(string: String): TextWithMarkup = TextWithMarkup(List(Part(string)))

  def fromHtml(string: String): TextWithMarkup = {
    val html = {
      val resultHolder = dom.document.createElement("span")
      resultHolder.innerHTML = string
      resultHolder
    }
    fromHtmlNodes(html)
  }

  def fromHtmlNodes(nodes: dom.raw.Node*): TextWithMarkup = {
    def ensureTrailingNewline(parts: Seq[Part]): Seq[Part] = parts match {
      case Seq()                                => Seq()
      case _ if !parts.last.text.endsWith("\n") => parts.updated(parts.size - 1, parts.last + "\n")
      case _                                    => parts
    }
    def fromHtmlNodesInner(nodes: Seq[dom.raw.Node], formatting: Formatting): Seq[Part] = {
      for {
        (node, i) <- nodes.zipWithIndex
        part <- {
          val last = i == nodes.length - 1
          parseNode(node) match {
            case ParsedNode.Text(string) => Seq(Part(string, formatting))
            case ParsedNode.Br(_)        => Seq(Part("\n", formatting))
            case ParsedNode.Div(e) if !last =>
              ensureTrailingNewline(fromHtmlNodesInner(children(e), formatting))
            case ParsedNode.P(e) if !last =>
              ensureTrailingNewline(fromHtmlNodesInner(children(e), formatting))
            case ParsedNode.B(e)    => fromHtmlNodesInner(children(e), formatting.copy(bold = true))
            case ParsedNode.I(e)    => fromHtmlNodesInner(children(e), formatting.copy(italic = true))
            case ParsedNode.Code(e) => fromHtmlNodesInner(children(e), formatting.copy(code = true))
            case ParsedNode.A(e) =>
              fromHtmlNodesInner(children(e), formatting.copy(link = Some(e.getAttribute("href"))))
            case _ => fromHtmlNodesInner(children(node), formatting)
          }
        }
      } yield part
    }

    val parts = fromHtmlNodesInner(nodes.toVector, formatting = Formatting.none)
    TextWithMarkup(parts.toList).canonicalized
  }

  // **************** public inner types **************** //
  case class Part(text: String, formatting: Formatting = Formatting.none) {

    def sub(beginOffset: Int, endOffset: Int = -1): Part =
      copy(
        text = if (endOffset == -1) text.substring(beginOffset) else text.substring(beginOffset, endOffset))

    def +(thatText: String): Part = copy(text = this.text + thatText)
  }

  case class Formatting(bold: Boolean = false,
                        italic: Boolean = false,
                        code: Boolean = false,
                        link: Option[String] = None)
  object Formatting {
    val none = Formatting()
  }

  // **************** private inner types **************** //
  private[TextWithMarkup] trait FormattingOption[T] {
    def getValue(part: Part): T
  }
  private[TextWithMarkup] object FormattingOption {
    private[TextWithMarkup] trait Applier[R] {
      def apply[T](option: FormattingOption[T], value: T, children: R, childrenParts: Iterable[Part]): R
    }

    object Bold extends FormattingOption[Boolean] {
      override def getValue(part: Part): Boolean = part.formatting.bold
    }
    object Italic extends FormattingOption[Boolean] {
      override def getValue(part: Part): Boolean = part.formatting.italic
    }
    object Code extends FormattingOption[Boolean] {
      override def getValue(part: Part): Boolean = part.formatting.code
    }
    object Link extends FormattingOption[Option[String]] {
      override def getValue(part: Part): Option[String] = part.formatting.link
    }
  }

  // **************** private helper methods **************** //
  private def serializeToDom[R](parts: List[Part],
                                applyFormattingOption: FormattingOption.Applier[R],
                                liftString: String => R,
                                mergeResults: Iterable[R] => R): R = {
    def serializeToDomInner(parts: Seq[Part], formattingLeft: List[FormattingOption[_]]): R = {
      formattingLeft match {
        case Nil => liftString(parts.map(_.text).mkString)
        case formattingOption :: otherFormattingOptions =>
          def inner[T](formattingOption: FormattingOption[T]): R = {
            var currentFormattingValue: T = null.asInstanceOf[T]
            val partBuffer = mutable.Buffer[Part]()
            val resultBuffer = mutable.Buffer[R]()

            def pushToBuffer(): Unit = {
              if (partBuffer.nonEmpty) {
                resultBuffer.append(
                  applyFormattingOption(
                    option = formattingOption,
                    value = currentFormattingValue,
                    children = serializeToDomInner(partBuffer.toList, otherFormattingOptions),
                    childrenParts = partBuffer
                  ))
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
                  partBuffer.append(part)
              }
              pushToBuffer()
            }
            mergeResults(resultBuffer)
          }
          inner(formattingOption)
      }
    }

    import FormattingOption._
    serializeToDomInner(parts, formattingLeft = List(Link, Code, Italic, Bold))
  }
}
