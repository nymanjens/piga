package app.flux.react.app.document

import app.common.testing.JsTestObjects._
import app.common.testing.TestObjects._
import app.models.document.Document.IndexedCursor
import app.models.document.Document.IndexedSelection
import app.models.document.TextWithMarkup
import app.models.document.TextWithMarkup.Formatting
import utest._

object DesktopTaskEditorTest extends TestSuite {

  override def tests = TestSuite {
    val editor = (new Module).desktopTaskEditor

    "convertToClipboardData" - {
      "covers multiple lines" - {
        editor.convertToClipboardData(
          newDocument(newTask("abc"), newTask("defg"), newTask("hij")),
          IndexedSelection(start = IndexedCursor(0, 1), end = IndexedCursor(2, 2))) ==>
          editor.ClipboardData(
            htmlText = "<ul><li>bc</li><li>defg</li><li>hi</li></ul>",
            plainText = "bc\ndefg\nhi")
      }
      "with formatting" - {
        editor.convertToClipboardData(
          newDocument(newTask(content = TextWithMarkup("a") + italic("b"))),
          IndexedSelection(start = IndexedCursor(0, 0), end = IndexedCursor(0, 2))
        ) ==>
          editor.ClipboardData(htmlText = "<ul><li>a<i>b</i></li></ul>", plainText = "ab")
      }
      "escapes html" - {
        editor.convertToClipboardData(
          newDocument(newTask("a<b>cd")),
          IndexedSelection(start = IndexedCursor(0, 0), end = IndexedCursor(0, 5))) ==>
          editor.ClipboardData(htmlText = "<ul><li>a&lt;b&gt;c</li></ul>", plainText = "a<b>c")
      }
      "converts newline to <br>" - {
        editor.convertToClipboardData(
          newDocument(newTask("a\nb")),
          IndexedSelection(start = IndexedCursor(0, 0), end = IndexedCursor(0, 3))) ==>
          editor.ClipboardData(htmlText = "<ul><li>a<br />b</li></ul>", plainText = "a\nb")
      }
      "handles indentation" - {
        editor.convertToClipboardData(
          newDocument(
            newTask("a", indentation = 2),
            newTask("b", indentation = 4),
            newTask("c", indentation = 1)),
          IndexedSelection(start = IndexedCursor(0, 0), end = IndexedCursor(2, 1))
        ) ==>
          editor.ClipboardData(
            htmlText = removeWhitespace("""
              <ul>
                <ul>
                  <li>a</li>
                  <ul>
                    <ul>
                      <li>b</li>
                    </ul>
                  </ul>
                </ul>
                <li>c</li>
              </ul>
            """),
            plainText = "a\nb\nc"
          )
      }
    }
    "clipboardStringToReplacement" - {
      def replacement(firstPartContent: TextWithMarkup, parts: editor.Replacement.Part*) =
        editor.Replacement.create(firstPartContent, parts: _*)
      def replacementPart(content: String, indentation: Int = 0) =
        editor.Replacement.Part(TextWithMarkup(content), indentation)
      def replacementPartFormatted(content: TextWithMarkup, indentation: Int = 0) =
        editor.Replacement.Part(content, indentation)
      def asHtml(s: String) = editor.ClipboardData(htmlText = s, plainText = "")
      def asText(s: String) = editor.ClipboardData(htmlText = "", plainText = s)

      "htmlText input" - {
        "without list tags" - {
          "p and div" - {
            editor.clipboardStringToReplacement(
              asHtml(removeWhitespace("""
              <p>a<br />b</p>
              <div>c</div>
              d
            """)),
              baseFormatting = Formatting.none) ==>
              replacement(
                TextWithMarkup("a"),
                replacementPart("b"),
                replacementPart("c"),
                replacementPart("d"))
          }
          "br" - {
            editor
              .clipboardStringToReplacement(asHtml("abc<br/>def"), baseFormatting = Formatting.none) ==>
              replacement(TextWithMarkup("abc"), replacementPart("def"))
          }
          "newline" - {
            editor.clipboardStringToReplacement(asHtml("abc\ndef"), baseFormatting = Formatting.none) ==>
              replacement(TextWithMarkup("abc"), replacementPart("def"))
          }
          "ignores formatting" - {
            editor
              .clipboardStringToReplacement(asHtml("<b>abc</b>"), baseFormatting = Formatting.none) ==>
              replacement(TextWithMarkup("abc"))
          }
          "plain text" - {
            editor.clipboardStringToReplacement(
              asHtml("""
              |x
              |y
            """.stripMargin.trim),
              baseFormatting = Formatting.none) ==>
              replacement(TextWithMarkup("x"), replacementPart("y"))
          }
          "ignores html comments" - {
            editor
              .clipboardStringToReplacement(asHtml("<!-- comment -->abc"), baseFormatting = Formatting.none) ==>
              replacement(TextWithMarkup("abc"))
          }
          "ignores style tags" - {
            editor
              .clipboardStringToReplacement(
                asHtml("""<style type="text/css">STYLE</style>abc"""),
                baseFormatting = Formatting.none) ==>
              replacement(TextWithMarkup("abc"))
          }
        }
        "with list tags" - {
          "single level" - {
            editor.clipboardStringToReplacement(
              asHtml(removeWhitespace("""
             <ul>
               <li>
                 <p>a<br />b</p>
                 <div>c</div>
               </li>
               <li>xyz</li>
             </ul>
            """)),
              baseFormatting = Formatting.none
            ) ==>
              replacement(TextWithMarkup("a\nb\nc"), replacementPart("xyz"))
          }
        }
        "inside and outside list tags" - {
          editor.clipboardStringToReplacement(
            asHtml(removeWhitespace("""
             a<i>b</i>c
             <ul>
               <li>
                 d<i>e</i>f
               </li>
             </ul>
            """)),
            baseFormatting = Formatting.none
          ) ==>
            replacement(
              TextWithMarkup("abc"),
              replacementPartFormatted(TextWithMarkup("d") + italic("e") + TextWithMarkup("f")))
        }
        "with baseFormatting" - {
          editor
            .clipboardStringToReplacement(asHtml("abc"), baseFormatting = Formatting(italic = true)) ==>
            replacement(italic("abc"))
        }
      }
      "plainText input" - {
        "newline" - {
          editor.clipboardStringToReplacement(asText("abc\ndef"), baseFormatting = Formatting.none) ==>
            replacement(TextWithMarkup("abc"), replacementPart("def"))
        }
        "with HTML tag" - {
          editor
            .clipboardStringToReplacement(asText("abc<br/>def"), baseFormatting = Formatting.none) ==>
            replacement(TextWithMarkup("abc<br/>def"))
        }
        "with baseFormatting" - {
          editor
            .clipboardStringToReplacement(asText("abc"), baseFormatting = Formatting(italic = true)) ==>
            replacement(italic("abc"))
        }
      }
    }
    "convertToClipboardData(clipboardStringToReplacement)" - {
      def asHtml(s: String) = editor.ClipboardData(htmlText = s, plainText = "")
      def roundTrip(html: String): Unit = {
        val replacement =
          editor.clipboardStringToReplacement(asHtml(html), baseFormatting = Formatting.none)
        val clipboardData = editor.convertToClipboardData(
          newDocument(
            replacement.parts.map(
              p =>
                newTask(
                  content = p.content,
                  orderToken = orderTokenA,
                  indentation = 10 + p.indentationRelativeToCurrent)): _*),
          IndexedSelection(
            start = IndexedCursor(0, 0),
            end = IndexedCursor(replacement.parts.length - 1, replacement.parts.last.contentString.length))
        )
        clipboardData.htmlText ==> html
      }
      "covers multiple lines" - {
        roundTrip("<ul><li>bc</li><li>defg</li><li>hi</li></ul>")
      }
      "with formatting" - {
        roundTrip("<ul><li><b>this is bold</b></li></ul>")
        roundTrip("<ul><li><i>this is italic</i></li></ul>")
        roundTrip("<ul><li><code>this is code</code></li></ul>")
        roundTrip("<ul><li><s>striked through</s></li></ul>")
        roundTrip("""<ul><li><a href="http://example.com">this is a link</a></li></ul>""")
      }
      "escapes html" - {
        roundTrip("<ul><li>a&lt;b&gt;c</li></ul>")
      }
      "converts newline to <br>" - {
        roundTrip("<ul><li>a<br />b</li></ul>")
      }
      "handles indentation" - {
        roundTrip("""
              <ul>
                <ul>
                  <li>a</li>
                  <ul>
                    <ul>
                      <li>b</li>
                    </ul>
                  </ul>
                </ul>
                <li>c</li>
              </ul>
            """.replace(" ", "").replace("\n", ""))
      }
    }
  }

  private def italic(string: String): TextWithMarkup = TextWithMarkup(string, Formatting(italic = true))

  private def removeWhitespace(s: String): String = s.replace(" ", "").replace("\n", "")

  private class Module extends app.common.testing.TestModule {
    val desktopTaskEditor = new DesktopTaskEditor
  }
}
