package app.models.document

import app.models.document.TextWithMarkup.Formatting
import utest._

object TextWithMarkupTest extends TestSuite {

  override def tests = TestSuite {
    "contentString" - {
      val twm = textWithMarkup("a") + italic("bc") + bold("d")
      twm.contentString ==> "abcd"
    }
    "isPlainText" - {
      "empty" - {
        "empty parts list" - {
          TextWithMarkup.empty.isPlainText ==> true
        }
        "empty string" - {
          textWithMarkup("").isPlainText ==> true
        }
        "empty string with markup" - {
          bold("").isPlainText ==> true
        }
      }
      "non-empty" - {
        "no markup" - {
          textWithMarkup("the apple is delicious").isPlainText ==> true
        }
        "markup" - {
          (textWithMarkup("A") + bold("B") + textWithMarkup("C")).isPlainText ==> false
        }
      }
    }
    "containsLink" - {
      "empty" - {
        textWithMarkup("").containsLink ==> false
      }
      "has no link" - {
        textWithMarkup("http is a protocol").containsLink ==> false
      }
      "has link" - {
        textWithMarkup(
          "the apple can be purchased at http://www.apples.org/ for a fair price"
        ).containsLink ==> true
      }
    }
    "sub" - {
      val twm = textWithMarkup("a") + italic("bc") + bold("d") + italic("efg")

      twm.sub(0, 0) ==> TextWithMarkup.empty
      twm.sub(3, 3) ==> TextWithMarkup.empty
      twm.sub(0) ==> twm

      twm.sub(3) ==> bold("d") + italic("efg")
      twm.sub(0, 3) ==> textWithMarkup("a") + italic("bc")
      twm.sub(0, 2) ==> textWithMarkup("a") + italic("b")
      twm.sub(5, 6) ==> italic("f")
      twm.sub(5, 7) ==> italic("fg")
    }
    "splitByNewlines" - {
      textWithMarkup("a\nb").splitByNewlines() ==> List(textWithMarkup("a"), textWithMarkup("b"))
      (textWithMarkup("ab") + bold("\nc")).splitByNewlines() ==> List(textWithMarkup("ab"), bold("c"))
      textWithMarkup("\nA").splitByNewlines() ==> List(TextWithMarkup.empty, textWithMarkup("A"))
      textWithMarkup("\n\nb").splitByNewlines() ==>
        List(TextWithMarkup.empty, TextWithMarkup.empty, textWithMarkup("b"))
    }
    "withFormatting" - {
      val twm = textWithMarkup("abc") + italic("def")

      twm.withFormatting(beginOffset = 1, endOffset = 4, _.copy(link = Some("example.com"))) ==>
        textWithMarkup("a") +
        textWithMarkup("bc", Formatting(link = Some("example.com"))) +
        textWithMarkup("d", Formatting(italic = true, link = Some("example.com"))) +
        italic("ef")
    }
    "withTransformedCharacters" - {
      val twm = textWithMarkup("abc") + italic("def")

      twm.withTransformedCharacters(beginOffset = 1, endOffset = 4, _.toUpperCase) ==>
        textWithMarkup("aBC") + italic("Def")
    }
    "formattingAtCursor" - {
      val twm = textWithMarkup("a") +
        textWithMarkup("bc", Formatting(italic = true, link = Some("example.com"))) +
        italic("d") +
        bold("e")

      twm.formattingAtCursor(0) ==> Formatting.none
      twm.formattingAtCursor(1) ==> Formatting.none
      twm.formattingAtCursor(2) ==> Formatting(italic = true, link = Some("example.com"))
      twm.formattingAtCursor(3) ==> Formatting(italic = true)
      twm.formattingAtCursor(4) ==> Formatting(italic = true)
      twm.formattingAtCursor(5) ==> Formatting(bold = true)
    }
    "anyLink" - {
      textWithMarkup("bc", Formatting(italic = true, link = Some("example.com"))).anyLink ==>
        Some("example.com")
    }
    "canonicalized equality check" - {
      textWithMarkup("a") + textWithMarkup("b") ==> textWithMarkup("ab")
      italic("a") + italic("b") ==> italic("ab")
      bold("abc").withFormatting(1, 2, _.copy(bold = true)) ==> bold("abc")
    }
    "toHtml" - {
      "br" - {
        textWithMarkup("A\n\nB").toHtml ==> "A<br /><br />B"
      }
      "b" - {
        (textWithMarkup("A") + bold("B") + textWithMarkup("C")).toHtml ==> "A<b>B</b>C"
      }
      "i" - {
        italic("ABC").toHtml ==> "<i>ABC</i>"
      }
      "b and i" - {
        (italic("AB") + textWithMarkup(
          "C",
          Formatting(bold = true, italic = true),
        )).toHtml ==> "<i>AB<b>C</b></i>"
      }
      "code" - {
        textWithMarkup("ABC", Formatting(code = true)).toHtml ==> "<code>ABC</code>"
      }
      "strikethrough" - {
        textWithMarkup("ABC", Formatting(strikethrough = true)).toHtml ==> "<s>ABC</s>"
      }
      "link" - {
        textWithMarkup("ABC", Formatting(link = Some("example.com"))).toHtml ==>
          """<a href="example.com">ABC</a>"""
      }
      "link and b" - {
        val twm =
          (textWithMarkup("ABC", Formatting(link = Some("example.com"))) + bold("D"))
            .withFormatting(2, 4, _.copy(bold = true))
        twm.toHtml ==> """<a href="example.com">AB<b>C</b></a><b>D</b>"""
      }
      "non-ascii" - {
        textWithMarkup("AäB").toHtml ==> "AäB"
      }
    }
    "toMarkdown" - {
      "newline" - {
        textWithMarkup("A\n\nB").toMarkdown ==> "A  \n  \nB"
      }
      "b" - {
        (textWithMarkup("A") + bold(" B ") + textWithMarkup("C")).toMarkdown ==> "A **B** C"
      }
      "i" - {
        italic("ABC").toMarkdown ==> "*ABC*"
      }
      "code" - {
        (textWithMarkup("X") + textWithMarkup("  ABC  ", Formatting(code = true))).toMarkdown ==> "X  `ABC`  "
      }
      "strikethrough" - {
        textWithMarkup("ABC \n", Formatting(strikethrough = true)).toMarkdown ==> "~ABC   \n~"
      }
      "link" - {
        textWithMarkup("ABC", Formatting(link = Some("example.com"))).toMarkdown ==> "[ABC](example.com)"
      }
      "link and b" - {
        val twm =
          (textWithMarkup("ABC", Formatting(link = Some("example.com"))) + bold("D"))
            .withFormatting(2, 4, _.copy(bold = true))
        twm.toMarkdown ==> "[AB**C**](example.com)**D**"
      }
      "non-ascii" - {
        textWithMarkup("AäB").toMarkdown ==> "AäB"
      }
    }
    "fromSanitizedHtml" - {
      "p" - {
        TextWithMarkup.fromSanitizedHtml("<p>A</p>") ==> textWithMarkup("A")
        TextWithMarkup.fromSanitizedHtml("<p>A</p><p>B</p>") ==> textWithMarkup("A\nB")
      }
      "div" - {
        TextWithMarkup.fromSanitizedHtml("<div>A</div>") ==> textWithMarkup("A")
        TextWithMarkup.fromSanitizedHtml("<div>A</div><div>B</div>") ==> textWithMarkup("A\nB")
      }
      "div and p" - {
        TextWithMarkup.fromSanitizedHtml("<div><p>A</p></div><div><p>B</p></div>") ==> textWithMarkup("A\nB")
      }
      "br" - {
        TextWithMarkup.fromSanitizedHtml("A<br/><br/>B") ==> textWithMarkup("A\n\nB")
      }
      "b" - {
        TextWithMarkup.fromSanitizedHtml("A<b>B</b>C") ==> textWithMarkup("A") + bold("B") + textWithMarkup(
          "C"
        )
      }
      "b via style" - {
        TextWithMarkup.fromSanitizedHtml("A<span style='font-weight:bold;'>B</span>C") ==> textWithMarkup(
          "A"
        ) + bold(
          "B"
        ) + textWithMarkup("C")
      }
      "i" - {
        TextWithMarkup.fromSanitizedHtml("<i>ABC</i>") ==> italic("ABC")
      }
      "i via style" - {
        TextWithMarkup.fromSanitizedHtml("<span style='font-style:italic'>ABC</style>") ==> italic("ABC")
      }
      "b and i" - {
        TextWithMarkup.fromSanitizedHtml("<i>AB<b>C</b></i>") ==>
          italic("AB") + textWithMarkup("C", Formatting(bold = true, italic = true))
      }
      "code" - {
        TextWithMarkup.fromSanitizedHtml("<code>ABC</code>") ==> textWithMarkup(
          "ABC",
          Formatting(code = true),
        )
      }
      "code via style" - {
        TextWithMarkup.fromSanitizedHtml("<span style='font: monospace;'>ABC</span>") ==> textWithMarkup(
          "ABC",
          Formatting(code = true),
        )
      }
      "strikethrough" - {
        TextWithMarkup.fromSanitizedHtml("<s>ABC</s>") ==> textWithMarkup(
          "ABC",
          Formatting(strikethrough = true),
        )
      }
      "strikethrough via style" - {
        TextWithMarkup.fromSanitizedHtml(
          "<span style='text-decoration:line-through'>ABC</span>"
        ) ==> textWithMarkup(
          "ABC",
          Formatting(strikethrough = true),
        )
      }
      "link" - {
        TextWithMarkup.fromSanitizedHtml("""<a href="example.com">ABC</a>""") ==>
          textWithMarkup("ABC", Formatting(link = Some("example.com")))
      }
      "ignores irrelevant elements" - {
        TextWithMarkup.fromSanitizedHtml("A<span>B</span>") ==> textWithMarkup("AB")
      }
      "non-ascii" - {
        TextWithMarkup.fromSanitizedHtml("AäB") ==> textWithMarkup("AäB")
      }
    }
  }

  private def italic(string: String): TextWithMarkup = textWithMarkup(string, Formatting(italic = true))
  private def bold(string: String): TextWithMarkup = textWithMarkup(string, Formatting(bold = true))
  private def textWithMarkup(string:String, formatting: Formatting= Formatting.none): TextWithMarkup = TextWithMarkup.create(string, formatting, alreadySanitized = true)
}
