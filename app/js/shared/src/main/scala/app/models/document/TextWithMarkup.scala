package app.models.document

import app.models.document.TextWithMarkup.Formatting
import app.models.document.TextWithMarkup.FormattingOption
import app.models.document.TextWithMarkup.Part
import hydro.common.DomNodeUtils.children
import hydro.common.DomNodeUtils._
import hydro.common.JsLoggingUtils.LogExceptionsCallback
import hydro.jsfacades.escapeHtml
import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom

import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

final class TextWithMarkup private (private val parts: List[Part]) {

  private val urlRegex: Regex = raw"https?:\/\/[^\s/$$.?#].[^\s]*".r
  private val shortLinkRegex: Regex = (raw"(((cr)|(cl)|b)\/\d{6,})|" +
    raw"((go|bit\.ly)\/[a-zA-Z0-9-_:]{3,})").r

  lazy val contentString: String = parts.map(_.text).mkString

  def isEmpty: Boolean = contentString.isEmpty
  def nonEmpty: Boolean = contentString.nonEmpty
  def isPlainText: Boolean = contentString.isEmpty || this == TextWithMarkup(contentString)
  lazy val containsLink: Boolean = urlRegex.findFirstMatchIn(contentString).isDefined

  lazy val toVdomNode: VdomNode = {
    var keyCounter = 0
    def nextKey: String = {
      keyCounter += 1
      "toVdomNode-cnt-" + keyCounter
    }

    def addSpaceAfterTrailingNewline(parts: List[Part]): List[Part] = {
      if (parts.nonEmpty && parts.last.text.endsWith("\n")) {
        // Fix for tailing newline issue. The last \n is seemingly ignored unless a non-empty element is trailing
        parts.updated(parts.length - 1, parts.last + " ")
      } else {
        parts
      }
    }

    def linkVdomNode(href: String, children: VdomNode): VdomNode = {
      <.a(
        ^.href := href,
        ^.target := "blank",
        ^.key := nextKey,
        ^.onClick ==> { e =>
          LogExceptionsCallback {
            if (e.ctrlKey) {
              e.preventDefault()
              dom.window.open(href, "_blank")
            }
          }.void
        },
        children
      )
    }

    val applyFormattingOption = new FormattingOption.Applier[VdomNode] {

      override def apply[T](
          option: FormattingOption[T],
          value: T,
          children: VdomNode,
          childrenParts: Iterable[Part],
      ): VdomNode = {
        def asBoolean(t: T): Boolean = t.asInstanceOf[Boolean]
        def asOption(t: T): Option[String] = t.asInstanceOf[Option[String]]

        option match {
          case FormattingOption.Bold   => if (asBoolean(value)) <.b(^.key := nextKey, children) else children
          case FormattingOption.Italic => if (asBoolean(value)) <.i(^.key := nextKey, children) else children
          case FormattingOption.Code   => if (asBoolean(value)) <.code(^.key := nextKey, children) else children
          case FormattingOption.Strikethrough =>
            if (asBoolean(value)) <.s(^.key := nextKey, children) else children
          case FormattingOption.Link =>
            asOption(value) match {
              case Some(href) => linkVdomNode(href = href, children = children)
              case None       => children
            }
        }
      }
    }

    def convertLinksToAnchors(string: String, insideLink: Boolean): VdomNode = {
      def inner(string: String): VdomNode = {
        def recurseWithMatch(
            m: Match,
            prependHttp: Boolean = false,
            recurseFirstPart: Boolean = false,
        ): VdomNode = {
          val before = string.substring(0, m.start)
          val linkText = m.group(0)
          val after = string.substring(m.end)
          <.span(
            ^.key := nextKey,
            if (recurseFirstPart) inner(before) else before,
            linkVdomNode(href = (if (prependHttp) "http://" else "") + linkText, children = linkText),
            inner(after)
          )
        }

        urlRegex.findFirstMatchIn(string) match {
          case Some(m) => recurseWithMatch(m, recurseFirstPart = true)
          case None =>
            shortLinkRegex.findFirstMatchIn(string) match {
              case Some(m) => recurseWithMatch(m, prependHttp = true)
              case None    => string
            }
        }
      }
      if (insideLink) string else inner(string)
    }

    TextWithMarkup.serializeToDom[VdomNode](
      addSpaceAfterTrailingNewline(parts),
      applyFormattingOption = applyFormattingOption,
      liftString = convertLinksToAnchors,
      mergeResults = _.toVdomArray)
  }

  def toHtml: String = {
    val applyFormattingOption = new FormattingOption.Applier[String] {
      override def apply[T](
          option: FormattingOption[T],
          value: T,
          children: String,
          childrenParts: Iterable[Part],
      ): String = {
        def asBoolean(t: T): Boolean = t.asInstanceOf[Boolean]
        def asOption(t: T): Option[String] = t.asInstanceOf[Option[String]]

        option match {
          case FormattingOption.Bold          => if (asBoolean(value)) s"<b>$children</b>" else children
          case FormattingOption.Italic        => if (asBoolean(value)) s"<i>$children</i>" else children
          case FormattingOption.Code          => if (asBoolean(value)) s"<code>$children</code>" else children
          case FormattingOption.Strikethrough => if (asBoolean(value)) s"<s>$children</s>" else children
          case FormattingOption.Link =>
            if (asOption(value).isDefined) s"""<a href="${asOption(value).get}">$children</a>""" else children
        }
      }
    }

    TextWithMarkup.serializeToDom[String](
      parts,
      applyFormattingOption = applyFormattingOption,
      liftString = (s, insideLink) => escapeHtml(s).replace("\n", "<br />"),
      mergeResults = _.mkString)
  }

  def +(that: TextWithMarkup): TextWithMarkup = TextWithMarkup.createCanonical(this.parts ++ that.parts)

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
      TextWithMarkup.createCanonical(
        subInner(parts, beginOffset, endOffset = if (endOffset == -1) contentString.length else endOffset))
    }
  }

  def splitByNewlines(): List[TextWithMarkup] = {
    val newlineIndex = contentString.indexOf('\n')
    if (newlineIndex >= 0) {
      this.sub(0, newlineIndex) :: this.sub(newlineIndex + 1).splitByNewlines()
    } else {
      this :: Nil
    }
  }

  def withFormatting(
      beginOffset: Int,
      endOffset: Int,
      updateFunc: Formatting => Formatting,
  ): TextWithMarkup = {
    def updated(textWithMarkup: TextWithMarkup): TextWithMarkup = {
      TextWithMarkup.createCanonical(
        textWithMarkup.parts.map(part => part.copy(formatting = updateFunc(part.formatting))))
    }
    sub(0, beginOffset) + updated(sub(beginOffset, endOffset)) + sub(endOffset, contentString.length)
  }

  def withTransformedCharacters(
      beginOffset: Int,
      endOffset: Int,
      characterTransform: String => String,
  ): TextWithMarkup = {
    def updated(textWithMarkup: TextWithMarkup): TextWithMarkup = {
      TextWithMarkup.createCanonical(
        textWithMarkup.parts.map(part => part.copy(text = characterTransform(part.text))))
    }
    sub(0, beginOffset) + updated(sub(beginOffset, endOffset)) + sub(endOffset, contentString.length)
  }

  def anyLink: Option[String] = parts.toStream.flatMap(_.formatting.link).headOption

  // **************** Object methods **************** //
  override def equals(obj: scala.Any): Boolean = obj match {
    case that: TextWithMarkup => this.parts == that.parts
    case _                    => false
  }
  override def hashCode(): Int = parts.hashCode()
  override def toString: String = toHtml
}

object TextWithMarkup {

  val empty: TextWithMarkup = new TextWithMarkup(Nil)

  def apply(string: String, formatting: Formatting = Formatting.none): TextWithMarkup =
    new TextWithMarkup(List(Part(string, formatting)))

  def fromHtml(string: String): TextWithMarkup = {
    val html = {
      val resultHolder = dom.document.createElement("span")
      resultHolder.innerHTML = string
      resultHolder
    }
    fromHtmlNodes(Seq(html))
  }

  def fromHtmlNodes(
      nodes: Iterable[dom.raw.Node],
      baseFormatting: Formatting = Formatting.none,
  ): TextWithMarkup = {
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
            case ParsedNode.S(e)    => fromHtmlNodesInner(children(e), formatting.copy(strikethrough = true))
            case ParsedNode.A(e) =>
              fromHtmlNodesInner(children(e), formatting.copy(link = Some(e.getAttribute("href"))))
            case ParsedNode.Style(e) => Seq() // Ignore style tags
            case _                   => fromHtmlNodesInner(children(node), formatting)
          }
        }
      } yield part
    }

    val parts = fromHtmlNodesInner(nodes.toVector, formatting = baseFormatting)
    TextWithMarkup.createCanonical(parts.toList)
  }

  // **************** public inner types **************** //
  case class Formatting(
      bold: Boolean = false,
      italic: Boolean = false,
      code: Boolean = false,
      strikethrough: Boolean = false,
      link: Option[String] = None,
  )
  object Formatting {
    val none = Formatting()
  }

  // **************** private inner types **************** //
  private case class Part(text: String, formatting: Formatting = Formatting.none) {

    def sub(beginOffset: Int, endOffset: Int = -1): Part =
      copy(
        text = if (endOffset == -1) text.substring(beginOffset) else text.substring(beginOffset, endOffset))

    def +(thatText: String): Part = copy(text = this.text + thatText)
  }

  private[TextWithMarkup] abstract class FormattingOption[T] {
    def getValue(part: Part): T
    override def toString: String = getClass.getSimpleName
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
    object Strikethrough extends FormattingOption[Boolean] {
      override def getValue(part: Part): Boolean = part.formatting.strikethrough
    }
    object Link extends FormattingOption[Option[String]] {
      override def getValue(part: Part): Option[String] = part.formatting.link
    }
  }

  // **************** private helper methods **************** //
  private def createCanonical(parts: Iterable[Part]): TextWithMarkup = {
    def createCanonicalInner(parts: List[Part]): List[Part] = parts match {
      case part1 :: part2 :: rest if part1.formatting == part2.formatting =>
        createCanonicalInner(Part(part1.text + part2.text, part1.formatting) :: rest)
      case Nil          => Nil
      case part :: rest => part :: createCanonicalInner(rest)
    }
    new TextWithMarkup(createCanonicalInner(parts.toList))
  }

  private type InsideLink = Boolean

  private def serializeToDom[R](
      parts: List[Part],
      applyFormattingOption: FormattingOption.Applier[R],
      liftString: (String, InsideLink) => R,
      mergeResults: Iterable[R] => R,
  ): R = {
    def serializeToDomInner(
        parts: Seq[Part],
        formattingLeft: List[FormattingOption[_]],
        insideLink: InsideLink,
    ): R = {
      formattingLeft match {
        case Nil => liftString(parts.map(_.text).mkString, insideLink)
        case formattingOption :: otherFormattingOptions =>
          def inner[T](formattingOption: FormattingOption[T]): R = {
            var currentFormattingValue: T = null.asInstanceOf[T]
            val partBuffer = mutable.Buffer[Part]()
            val resultBuffer = mutable.Buffer[R]()

            def currentFormattingIsLink: Boolean =
              formattingOption == FormattingOption.Link &&
                currentFormattingValue.asInstanceOf[Option[String]].isDefined
            def pushToBuffer(): Unit = {
              if (partBuffer.nonEmpty) {
                resultBuffer.append(
                  applyFormattingOption(
                    option = formattingOption,
                    value = currentFormattingValue,
                    children = serializeToDomInner(
                      partBuffer.toList,
                      otherFormattingOptions,
                      insideLink = insideLink || currentFormattingIsLink),
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
            }
            pushToBuffer()
            mergeResults(resultBuffer)
          }
          inner(formattingOption)
      }
    }

    import FormattingOption._
    serializeToDomInner(
      parts,
      formattingLeft = List(Link, Code, Strikethrough, Italic, Bold),
      insideLink = false)
  }
}
