package app.flux.react.app.document

import app.common.testing.TestObjects._
import app.common.testing.JsTestObjects._
import hydro.common.time.LocalDateTime
import app.models.document.Document.IndexedCursor
import app.models.document.Document.IndexedSelection
import app.models.document.TextWithMarkup.Formatting
import app.models.document.Task
import app.models.document.TextWithMarkup
import hydro.scala2js.StandardConverters._
import app.scala2js.AppConverters._
import utest._

import scala.collection.immutable.Seq

object TaskEditorTest extends TestSuite {

  override def tests = TestSuite {
    val taskEditor = (new Module).taskEditor

    "convertToClipboardData" - {
      "covers multiple lines" - {
        taskEditor.convertToClipboardData(
          newDocument(newTask("abc"), newTask("defg"), newTask("hij")),
          IndexedSelection(start = IndexedCursor(0, 1), end = IndexedCursor(2, 2))) ==>
          taskEditor.ClipboardData(
            htmlText = "<ul><li>bc</li><li>defg</li><li>hi</li></ul>",
            plainText = "bc\ndefg\nhi")
      }
      "with formatting" - {
        taskEditor.convertToClipboardData(
          newDocument(newTask(content = TextWithMarkup("a") + italic("b"))),
          IndexedSelection(start = IndexedCursor(0, 0), end = IndexedCursor(0, 2))
        ) ==>
          taskEditor.ClipboardData(htmlText = "<ul><li>a<i>b</i></li></ul>", plainText = "ab")
      }
      "escapes html" - {
        taskEditor.convertToClipboardData(
          newDocument(newTask("a<b>cd")),
          IndexedSelection(start = IndexedCursor(0, 0), end = IndexedCursor(0, 5))) ==>
          taskEditor.ClipboardData(htmlText = "<ul><li>a&lt;b&gt;c</li></ul>", plainText = "a<b>c")
      }
      "converts newline to <br>" - {
        taskEditor.convertToClipboardData(
          newDocument(newTask("a\nb")),
          IndexedSelection(start = IndexedCursor(0, 0), end = IndexedCursor(0, 3))) ==>
          taskEditor.ClipboardData(htmlText = "<ul><li>a<br />b</li></ul>", plainText = "a\nb")
      }
      "handles indentation" - {
        taskEditor.convertToClipboardData(
          newDocument(
            newTask("a", indentation = 2),
            newTask("b", indentation = 4),
            newTask("c", indentation = 1)),
          IndexedSelection(start = IndexedCursor(0, 0), end = IndexedCursor(2, 1))
        ) ==>
          taskEditor.ClipboardData(
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
      def replacement(firstPartContent: TextWithMarkup, parts: taskEditor.Replacement.Part*) =
        taskEditor.Replacement.create(firstPartContent, parts: _*)
      def replacementPart(content: String, indentation: Int = 0) =
        taskEditor.Replacement.Part(TextWithMarkup(content), indentation)
      def replacementPartFormatted(content: TextWithMarkup, indentation: Int = 0) =
        taskEditor.Replacement.Part(content, indentation)

      "without list tags" - {
        "p and div" - {
          taskEditor.clipboardStringToReplacement(
            removeWhitespace("""
              <p>a<br />b</p>
              <div>c</div>
              d
            """),
            baseFormatting = Formatting.none) ==>
            replacement(TextWithMarkup("a"), replacementPart("b"), replacementPart("c"), replacementPart("d"))
        }
        "br" - {
          taskEditor.clipboardStringToReplacement("abc<br/>def", baseFormatting = Formatting.none) ==>
            replacement(TextWithMarkup("abc"), replacementPart("def"))
        }
        "newline" - {
          taskEditor.clipboardStringToReplacement("abc\ndef", baseFormatting = Formatting.none) ==>
            replacement(TextWithMarkup("abc"), replacementPart("def"))
        }
        "ignores formatting" - {
          taskEditor.clipboardStringToReplacement("<b>abc</b>", baseFormatting = Formatting.none) ==>
            replacement(TextWithMarkup("abc"))
        }
        "plain text" - {
          taskEditor.clipboardStringToReplacement(
            """
              |x
              |y
            """.stripMargin.trim,
            baseFormatting = Formatting.none) ==>
            replacement(TextWithMarkup("x"), replacementPart("y"))
        }
      }
      "with list tags" - {
        "single level" - {
          taskEditor.clipboardStringToReplacement(
            removeWhitespace("""
             <ul>
               <li>
                 <p>a<br />b</p>
                 <div>c</div>
               </li>
               <li>xyz</li>
             </ul>
            """),
            baseFormatting = Formatting.none
          ) ==>
            replacement(TextWithMarkup("a\nb\nc"), replacementPart("xyz"))
        }
      }
      "inside and outside list tags" - {
        taskEditor.clipboardStringToReplacement(
          removeWhitespace("""
             a<i>b</i>c
             <ul>
               <li>
                 d<i>e</i>f
               </li>
             </ul>
            """),
          baseFormatting = Formatting.none
        ) ==>
          replacement(
            TextWithMarkup("abc"),
            replacementPartFormatted(TextWithMarkup("d") + italic("e") + TextWithMarkup("f")))
      }
    }
    "convertToClipboardData(clipboardStringToReplacement)" - {
      def roundTrip(html: String): Unit = {
        val replacement = taskEditor.clipboardStringToReplacement(html, baseFormatting = Formatting.none)
        val clipboardData = taskEditor.convertToClipboardData(
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

  private class Module extends common.testing.TestModule {
    val taskEditor = new TaskEditor
  }
}
