package flux.react.app.desktop

import flux.react.app.desktop.TextWithMarkup.{Formatting, Part}
import scala2js.Converters._
import utest._

object TextWithMarkupTest extends TestSuite {

  override def tests = TestSuite {
    "contentString" - {
      val textWithMarkup = TextWithMarkup("a") + italic("bc") + bold("d")
      textWithMarkup.contentString ==> "abcd"
    }
    "sub" - {
      val textWithMarkup = TextWithMarkup("a") + italic("bc") + bold("d") + italic("efg")

      textWithMarkup.sub(0, 0) ==> TextWithMarkup.empty
      textWithMarkup.sub(3, 3) ==> TextWithMarkup.empty
      textWithMarkup.sub(0) ==> textWithMarkup

      textWithMarkup.sub(3) ==> bold("d") + italic("efg")
      textWithMarkup.sub(0, 3) ==> TextWithMarkup("a") + italic("bc")
      textWithMarkup.sub(0, 2) ==> TextWithMarkup("a") + italic("b")
      textWithMarkup.sub(5, 6) ==> italic("f")
      textWithMarkup.sub(5, 7) ==> italic("fg")
    }
    "withFormatting" - {
      val textWithMarkup = TextWithMarkup("abc") + italic("def")

      textWithMarkup.withFormatting(beginOffset = 1, endOffset = 4, _.copy(link = Some("example.com"))) ==>
        TextWithMarkup("a") +
          TextWithMarkup("bc", Formatting(link = Some("example.com"))) +
          TextWithMarkup("d", Formatting(italic = true, link = Some("example.com"))) +
          italic("ef")
    }
    "formattingAtCursor" - {
      val textWithMarkup = TextWithMarkup("a") +
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
    "anyLink" - {
      TextWithMarkup("bc", Formatting(italic = true, link = Some("example.com"))).anyLink ==>
        Some("example.com")
    }
    "canonicalized equality check" - {
      TextWithMarkup("a") + TextWithMarkup("b") ==> TextWithMarkup("ab")
      italic("a") + italic("b") ==> italic("ab")
      bold("abc").withFormatting(1, 2, _.copy(bold = true)) ==> bold("abc")
    }
    "toHtml" - {
      "br" - {
        TextWithMarkup("A\n\nB").toHtml ==> "A<br /><br />B"
      }
      "b" - {
        (TextWithMarkup("A") + bold("B") + TextWithMarkup("C")).toHtml ==> "A<b>B</b>C"
      }
      "i" - {
        italic("ABC").toHtml ==> "<i>ABC</i>"
      }
      "b and i" - {
        (italic("AB") + TextWithMarkup("C", Formatting(bold = true, italic = true))).toHtml ==> "<i>AB<b>C</b></i>"
      }
      "code" - {
        TextWithMarkup("ABC", Formatting(code = true)).toHtml ==> "<code>ABC</code>"
      }
      "link" - {
        TextWithMarkup("ABC", Formatting(link = Some("example.com"))).toHtml ==>
          """<a href="example.com">ABC</a>"""
      }
      "link and b" - {
        val textWithMarkup =
          (TextWithMarkup("ABC", Formatting(link = Some("example.com"))) + bold("D"))
            .withFormatting(2, 4, _.copy(bold = true))
        textWithMarkup.toHtml ==> """<a href="example.com">AB<b>C</b></a><b>D</b>"""
      }
    }
    "fromHtml" - {
      "p" - {
        TextWithMarkup.fromHtml("<p>A</p>") ==> TextWithMarkup("A")
        TextWithMarkup.fromHtml("<p>A</p><p>B</p>") ==> TextWithMarkup("A\nB")
      }
      "div" - {
        TextWithMarkup.fromHtml("<div>A</div>") ==> TextWithMarkup("A")
        TextWithMarkup.fromHtml("<div>A</div><div>B</div>") ==> TextWithMarkup("A\nB")
      }
      "div and p" - {
        TextWithMarkup.fromHtml("<div><p>A</p></div><div><p>B</p></div>") ==> TextWithMarkup("A\nB")
      }
      "br" - {
        TextWithMarkup.fromHtml("A<br/><br/>B") ==> TextWithMarkup("A\n\nB")
      }
      "b" - {
        TextWithMarkup.fromHtml("A<b>B</b>C") ==> TextWithMarkup("A") + bold("B") + TextWithMarkup("C")
      }
      "i" - {
        TextWithMarkup.fromHtml("<i>ABC</i>") ==> italic("ABC")
      }
      "b and i" - {
        TextWithMarkup.fromHtml("<i>AB<b>C</b></i>") ==>
          italic("AB") + TextWithMarkup("C", Formatting(bold = true, italic = true))
      }
      "code" - {
        TextWithMarkup.fromHtml("<code>ABC</code>") ==> TextWithMarkup("ABC", Formatting(code = true))
      }
      "link" - {
        TextWithMarkup.fromHtml("""<a href="example.com">ABC</a>""") ==>
          TextWithMarkup("ABC", Formatting(link = Some("example.com")))
      }
      "ignores irrelevant elements" - {
        TextWithMarkup.fromHtml("A<span>B</span>") ==> TextWithMarkup("AB")
      }
    }
  }

  private def italic(string: String): TextWithMarkup = TextWithMarkup(string, Formatting(italic = true))
  private def bold(string: String): TextWithMarkup = TextWithMarkup(string, Formatting(bold = true))
}
