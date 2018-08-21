package flux.react.app.desktop

import common.testing.JsTestObjects._
import flux.react.app.desktop.TaskSequence.{IndexedCursor, IndexedSelection}
import flux.react.app.desktop.TextWithMarkup.{Formatting, Part, withoutFormatting}
import scala2js.Converters._
import utest._

import scala.collection.immutable.Seq

object TextWithMarkupTest extends TestSuite {

  override def tests = TestSuite {
    "contentString" - {
      val textWithMarkup = withoutFormatting("a") + italic("bc") + bold("d")
      textWithMarkup.contentString ==> "abcd"
    }
    "+" - {
      val textWithMarkup = withoutFormatting("a") + italic("bc") + bold("d")
      textWithMarkup ==>
        TextWithMarkup(
          List(Part("a"), Part("bc", Formatting(italic = true)), Part("d", Formatting(bold = true))))
    }
    "sub" - {
      val textWithMarkup = withoutFormatting("a") + italic("bc") + bold("d") + italic("efg")

      textWithMarkup.sub(0, 0) ==> TextWithMarkup.empty
      textWithMarkup.sub(3, 3) ==> TextWithMarkup.empty
      textWithMarkup.sub(0) ==> textWithMarkup

      textWithMarkup.sub(3) ==> bold("d") + italic("efg")
      textWithMarkup.sub(0, 3) ==> withoutFormatting("a") + italic("bc")
      textWithMarkup.sub(0, 2) ==> withoutFormatting("a") + italic("b")
      textWithMarkup.sub(5, 6) ==> italic("f")
      textWithMarkup.sub(5, 7) ==> italic("fg")
    }
    "withFormatting" - {
      val textWithMarkup = withoutFormatting("abc") + italic("def")

      textWithMarkup.withFormatting(beginOffset = 1, endOffset = 4, _.copy(link = Some("example.com"))) ==>
        TextWithMarkup("a") +
          TextWithMarkup("bc", Formatting(link = Some("example.com"))) +
          TextWithMarkup("d", Formatting(italic = true, link = Some("example.com"))) +
          italic("ef")
    }
    "formattingAtCursor" - {
      val textWithMarkup = withoutFormatting("a") +
        TextWithMarkup("bc", Formatting(italic = true, link = Some("example.com"))) +
        italic("d") +
        bold("e")

      textWithMarkup.formattingAtCursor(0) ==> Formatting.none
      textWithMarkup.formattingAtCursor(1) ==> Formatting.none
      textWithMarkup.formattingAtCursor(2) ==> Formatting(italic = true, link = Some("example.com"))
      textWithMarkup.formattingAtCursor(3) ==> Formatting(italic = true)
      textWithMarkup.formattingAtCursor(4) ==> Formatting(italic = true)
      textWithMarkup.formattingAtCursor(5) ==> Formatting(bold = true)
    }
    "canonicalized" - {
      // TODO
    }
    "toHtml" - {
      // TODO
    }
    "fromHtml" - {
      // TODO
    }
  }

  private def italic(string: String): TextWithMarkup =
    TextWithMarkup(List(Part(string, Formatting(italic = true))))
  private def bold(string: String): TextWithMarkup =
    TextWithMarkup(List(Part(string, Formatting(bold = true))))
}
