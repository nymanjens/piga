package app.models.document

import app.models.document.TextWithMarkup.Formatting
import utest._

object TextWithMarkupTest extends TestSuite {

  override def tests = TestSuite {
    "contentString" - {
      val textWithMarkup = TextWithMarkup("a") + italic("bc") + bold("d")
      textWithMarkup.contentString ==> "abcd"
    }
    "isPlainText" - {
      "empty" - {
        "empty parts list" - {
          TextWithMarkup.empty.isPlainText ==> true
        }
        "empty string" - {
          TextWithMarkup("").isPlainText ==> true
        }
        "empty string with markup" - {
          bold("").isPlainText ==> true
        }
      }
      "non-empty" - {
        "no markup" - {
          TextWithMarkup("the apple is delicious").isPlainText ==> true
        }
        "markup" - {
          (TextWithMarkup("A") + bold("B") + TextWithMarkup("C")).isPlainText ==> false
        }
      }
    }
    "containsLink" - {
      "empty" - {
        TextWithMarkup("").containsLink ==> false
      }
      "has no link" - {
        TextWithMarkup("http is a protocol").containsLink ==> false
      }
      "has link" - {
        TextWithMarkup("the apple can be purchased at http://www.apples.org/ for a fair price").containsLink ==> true
      }
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
    "splitByNewlines" - {
      TextWithMarkup("a\nb").splitByNewlines() ==> List(TextWithMarkup("a"), TextWithMarkup("b"))
      (TextWithMarkup("ab") + bold("\nc")).splitByNewlines() ==> List(TextWithMarkup("ab"), bold("c"))
      TextWithMarkup("\nA").splitByNewlines() ==> List(TextWithMarkup.empty, TextWithMarkup("A"))
      TextWithMarkup("\n\nb").splitByNewlines() ==>
        List(TextWithMarkup.empty, TextWithMarkup.empty, TextWithMarkup("b"))
    }
    "withFormatting" - {
      val textWithMarkup = TextWithMarkup("abc") + italic("def")

      textWithMarkup.withFormatting(beginOffset = 1, endOffset = 4, _.copy(link = Some("example.com"))) ==>
        TextWithMarkup("a") +
          TextWithMarkup("bc", Formatting(link = Some("example.com"))) +
          TextWithMarkup("d", Formatting(italic = true, link = Some("example.com"))) +
          italic("ef")
    }
    "withTransformedCharacters" - {
      val textWithMarkup = TextWithMarkup("abc") + italic("def")

      textWithMarkup.withTransformedCharacters(beginOffset = 1, endOffset = 4, _.toUpperCase) ==>
        TextWithMarkup("aBC") + italic("Def")
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
      "strikethrough" - {
        TextWithMarkup("ABC", Formatting(strikethrough = true)).toHtml ==> "<s>ABC</s>"
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
      "non-ascii" - {
        TextWithMarkup("AäB").toHtml ==> "AäB"
      }
    }
    "toMarkdown" - {
      "newline" - {
        TextWithMarkup("A\n\nB").toMarkdown ==> "A  \n  \nB"
      }
      "b" - {
        (TextWithMarkup("A") + bold(" B ") + TextWithMarkup("C")).toMarkdown ==> "A **B** C"
      }
      "i" - {
        italic("ABC").toMarkdown ==> "*ABC*"
      }
      "code" - {
        (TextWithMarkup("X") + TextWithMarkup("  ABC  ", Formatting(code = true))).toMarkdown ==> "X  `ABC`  "
      }
      "strikethrough" - {
        TextWithMarkup("ABC \n", Formatting(strikethrough = true)).toMarkdown ==> "~ABC   \n~"
      }
      "link" - {
        TextWithMarkup("ABC", Formatting(link = Some("example.com"))).toMarkdown ==> "[ABC](example.com)"
      }
      "link and b" - {
        val textWithMarkup =
          (TextWithMarkup("ABC", Formatting(link = Some("example.com"))) + bold("D"))
            .withFormatting(2, 4, _.copy(bold = true))
        textWithMarkup.toMarkdown ==> "[AB**C**](example.com)**D**"
      }
      "non-ascii" - {
        TextWithMarkup("AäB").toMarkdown ==> "AäB"
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
      "strikethrough" - {
        TextWithMarkup.fromHtml("<s>ABC</s>") ==> TextWithMarkup("ABC", Formatting(strikethrough = true))
      }
      "link" - {
        TextWithMarkup.fromHtml("""<a href="example.com">ABC</a>""") ==>
          TextWithMarkup("ABC", Formatting(link = Some("example.com")))
      }
      "ignores irrelevant elements" - {
        TextWithMarkup.fromHtml("A<span>B</span>") ==> TextWithMarkup("AB")
      }
      "non-ascii" - {
        TextWithMarkup.fromHtml("AäB") ==> TextWithMarkup("AäB")
      }
    }
  }

  private def italic(string: String): TextWithMarkup = TextWithMarkup(string, Formatting(italic = true))
  private def bold(string: String): TextWithMarkup = TextWithMarkup(string, Formatting(bold = true))
}
