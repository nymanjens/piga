package app.flux.react.app.document

import scala.collection.immutable.Seq
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
          newDocument(
            newTask("abc", tags = Seq()),
            newTask("defg", tags = Seq()),
            newTask("hij", tags = Seq()),
          ),
          IndexedSelection(start = IndexedCursor(0, 1), end = IndexedCursor(2, 2))
        ) ==>
          editor.ClipboardData(
            htmlText = removeFormattingWhitespace("""
              <ul>
                <li piga="true">bc</li>
                <li piga="true">defg</li>
                <li piga="true">hi</li>
              </ul>
            """),
            plainText = "bc\ndefg\nhi",
          )
      }
      "with formatting" - {
        editor.convertToClipboardData(
          newDocument(
            newTask(content = TextWithMarkup("a") + italic("b"), tags = Seq()),
          ),
          IndexedSelection(start = IndexedCursor(0, 0), end = IndexedCursor(0, 2))
        ) ==>
          editor.ClipboardData(
            htmlText = removeFormattingWhitespace("""
              <span piga="true">
                a<i>b</i>
              </span>
            """),
            plainText = "ab",
          )
      }
      "collapsed and with tasks" - {
        "single task" - {
          "whole task is selected" - {
            editor.convertToClipboardData(
              newDocument(
                newTask("ABC", collapsed = true, tags = Seq("XX", "YY")),
              ),
              IndexedSelection(start = IndexedCursor(0, 0), end = IndexedCursor(0, 3))
            ) ==>
              editor.ClipboardData(
                htmlText = removeFormattingWhitespace("""
                    <span piga="true" piga-tags="XX,YY">
                      ABC
                    </span>
                  """),
                plainText = "ABC",
              )
          }
        }
        "multiple tasks" - {
          "whole tasks are selected" - {
            editor.convertToClipboardData(
              newDocument(
                newTask("ABC", collapsed = true, tags = Seq("XX", "YY")),
                newTask("DEF", collapsed = true, tags = Seq()),
              ),
              IndexedSelection(start = IndexedCursor(0, 0), end = IndexedCursor(1, 3))
            ) ==>
              editor.ClipboardData(
                htmlText = removeFormattingWhitespace("""
                    <ul>
                      <li piga="true" piga-collapsed="true" piga-tags="XX,YY">
                        ABC
                      </li>
                      <li piga="true" piga-collapsed="true">
                        DEF
                      </li>
                    </ul>
                  """),
                plainText = "ABC\nDEF",
              )
          }
          "tasks are partially selected" - {
            editor.convertToClipboardData(
              newDocument(
                newTask("ABC", collapsed = true, tags = Seq("XX", "YY")),
                newTask("DEF", collapsed = true, tags = Seq()),
              ),
              IndexedSelection(start = IndexedCursor(0, 1), end = IndexedCursor(1, 2))
            ) ==>
              editor.ClipboardData(
                htmlText = removeFormattingWhitespace("""
                    <ul>
                      <li piga="true" piga-collapsed="true">
                        BC
                      </li>
                      <li piga="true" piga-collapsed="true">
                        DE
                      </li>
                    </ul>
                  """),
                plainText = "BC\nDE",
              )
          }
        }
      }
      "escapes html" - {
        editor.convertToClipboardData(
          newDocument(
            newTask("a<b>cd", tags = Seq()),
          ),
          IndexedSelection(start = IndexedCursor(0, 0), end = IndexedCursor(0, 5))) ==>
          editor.ClipboardData(
            htmlText = removeFormattingWhitespace("""
              <span piga="true">
                a&lt;b&gt;c
              </span>
            """),
            plainText = "a<b>c",
          )
      }
      "converts newline to <br>" - {
        editor.convertToClipboardData(
          newDocument(
            newTask("a\nb", tags = Seq()),
          ),
          IndexedSelection(start = IndexedCursor(0, 0), end = IndexedCursor(0, 3))) ==>
          editor.ClipboardData(
            htmlText = removeFormattingWhitespace("""
              <span piga="true">
                a<br />b
              </span>
            """),
            plainText = "a\nb",
          )
      }
      "handles indentation" - {
        editor.convertToClipboardData(
          newDocument(
            newTask("a", indentation = 2, tags = Seq()),
            newTask("b", indentation = 4, tags = Seq()),
            newTask("c", indentation = 1, tags = Seq())),
          IndexedSelection(start = IndexedCursor(0, 0), end = IndexedCursor(2, 1))
        ) ==>
          editor.ClipboardData(
            htmlText = removeFormattingWhitespace("""
              <ul>
                <ul>
                  <li piga="true">a</li>
                  <ul>
                    <ul>
                      <li piga="true">b</li>
                    </ul>
                  </ul>
                </ul>
                <li piga="true">c</li>
              </ul>
            """),
            plainText = "a\nb\nc"
          )
      }
    }
    "clipboardStringToReplacement" - {
      def replacement(firstPartContent: TextWithMarkup, parts: editor.Replacement.Part*) =
        editor.Replacement.create(firstPartContent, parts: _*)
      def replacementFromParts(parts: editor.Replacement.Part*) = editor.Replacement(parts.toList)
      def replacementPart(
          content: String,
          indentation: Int = 0,
          collapsed: Boolean = false,
          tags: Seq[String] = Seq(),
      ) =
        editor.Replacement.Part(TextWithMarkup(content), indentation, collapsed, tags)
      def replacementPartFormatted(
          content: TextWithMarkup,
          indentation: Int = 0,
          collapsed: Boolean = false,
          tags: Seq[String] = Seq(),
      ) =
        editor.Replacement.Part(content, indentation, collapsed, tags)
      def asHtml(s: String) = editor.ClipboardData(htmlText = s, plainText = "")
      def asText(s: String) = editor.ClipboardData(htmlText = "", plainText = s)

      "htmlText input from piga" - {
        "without list tags" - {
          "br" - {
            editor.clipboardStringToReplacement(
              asHtml(removeFormattingWhitespace("""
                <span piga="true">
                  abc<br />def
                </span>
              """)),
              baseFormatting = Formatting.none,
            ) ==>
              replacement(TextWithMarkup("abc\ndef"))
          }
          "applies formatting" - {
            editor.clipboardStringToReplacement(
              asHtml(removeFormattingWhitespace("""
                <span piga="true">
                  a<i>b</i>c
                </span>
              """)),
              baseFormatting = Formatting.none,
            ) ==>
              replacement(TextWithMarkup("a") + italic("b") + TextWithMarkup("c"))
          }
          "with tags and collapsed" - {
            editor.clipboardStringToReplacement(
              asHtml(removeFormattingWhitespace("""
                <span piga="true" piga-collapsed="true" piga-tags="XX,YY">
                  a<i>b</i>c
                </span>
              """)),
              baseFormatting = Formatting.none,
            ) ==>
              replacementFromParts(
                replacementPartFormatted(
                  TextWithMarkup("a") + italic("b") + TextWithMarkup("c"),
                  collapsed = true,
                  tags = Seq("XX", "YY"),
                ))
          }
        }
        "with list tags" - {
          editor.clipboardStringToReplacement(
            asHtml(removeFormattingWhitespace("""
           <ul>
             <li piga="true">
               a<br />b
             </li>
             <li piga="true" piga-collapsed="true" piga-tags="XX,YY">
               xyz
             </li>
           </ul>
          """)),
            baseFormatting = Formatting.none
          ) ==>
            replacement(
              TextWithMarkup("a\nb"),
              replacementPart("xyz", collapsed = true, tags = Seq("XX", "YY")),
            )
        }
      }
      "htmlText input from outside piga" - {
        "without list tags" - {
          "p and div" - {
            editor.clipboardStringToReplacement(
              asHtml(removeFormattingWhitespace("""
              <p>a<br />b</p>
              <div>c</div>
              d
            """)),
              baseFormatting = Formatting.none
            ) ==>
              replacement(
                TextWithMarkup("a"),
                replacementPart("b"),
                replacementPart("c"),
                replacementPart("d"))
          }
          "br" - {
            editor.clipboardStringToReplacement(asHtml("abc<br/>def"), baseFormatting = Formatting.none) ==>
              replacement(TextWithMarkup("abc"), replacementPart("def"))
          }
          "newline" - {
            editor.clipboardStringToReplacement(asHtml("abc\ndef"), baseFormatting = Formatting.none) ==>
              replacement(TextWithMarkup("abc"), replacementPart("def"))
          }
          "applies formatting" - {
            editor.clipboardStringToReplacement(asHtml("a<i>b</i>c"), baseFormatting = Formatting.none) ==>
              replacement(TextWithMarkup("a") + italic("b") + TextWithMarkup("c"))
          }
          "ignores html comments" - {
            editor.clipboardStringToReplacement(
              asHtml("<!-- comment -->abc"),
              baseFormatting = Formatting.none) ==>
              replacement(TextWithMarkup("abc"))
          }
          "ignores style tags" - {
            editor.clipboardStringToReplacement(
              asHtml("""<style type="text/css">STYLE</style>abc"""),
              baseFormatting = Formatting.none) ==>
              replacement(TextWithMarkup("abc"))
          }
        }
        "with list tags" - {
          "single level" - {
            editor.clipboardStringToReplacement(
              asHtml(removeFormattingWhitespace("""
                <ul>
                  <li>
                    <p>a<br />b</p>
                    <div>c</div>
                  </li>
                  <li>xy<i>z</i></li>
                </ul>
              """)),
              baseFormatting = Formatting.none
            ) ==>
              replacement(
                TextWithMarkup("a\nb\nc"),
                replacementPartFormatted(TextWithMarkup("xy") + italic("z"), indentation = 0),
              )
          }
          "nested level" - {
            editor.clipboardStringToReplacement(
              asHtml(removeFormattingWhitespace("""
                <ul>
                  <li>abc</li>
                  <ul>
                    <li>def</li>
                    <li>
                      ghi
                      <ul>
                        <li>klm</li>
                      </ul>
                    </li>
                  </ul>
                </ul>
              """)),
              baseFormatting = Formatting.none
            ) ==>
              replacement(
                TextWithMarkup("abc"),
                replacementPart("def", indentation = 1),
                replacementPart("ghi", indentation = 1),
                replacementPart("klm", indentation = 2),
              )
          }
        }
        "inside and outside list tags" - {
          editor.clipboardStringToReplacement(
            asHtml(removeFormattingWhitespace("""
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
              TextWithMarkup("a") + italic("b") + TextWithMarkup("c"),
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
                  indentation = 10 + p.indentationRelativeToCurrent,
                  collapsed = p.collapsed,
                  tags = p.tags,
              )): _*),
          IndexedSelection(
            start = IndexedCursor(0, 0),
            end = IndexedCursor(replacement.parts.length - 1, replacement.parts.last.contentString.length))
        )
        clipboardData.htmlText ==> html
      }
      "covers multiple lines" - {
        roundTrip(removeFormattingWhitespace("""
          <ul>
            <li piga="true">bc</li>
            <li piga="true">defg</li>
            <li piga="true">hi</li>
          </ul>
        """))
      }
      "with formatting and tags" - {
        roundTrip(removeFormattingWhitespace("""
          <span piga="true" piga-tags="XX,YY">
            <b>this is bold</b>
            <i>this is italic</i>
            <code>this is code</code>
            <s>striked through</s>
            <a href="http://example.com">this is a link</a>
          </span>
        """))
      }
      "escapes html" - {
        roundTrip(removeFormattingWhitespace("""
          <span piga="true">
            a&lt;b&gt;c
          </span>
        """))
      }
      "converts newline to <br>" - {
        roundTrip(removeFormattingWhitespace("""
          <span piga="true">
            a<br />b
          </span>
        """))
      }
      "handles indentation, collapsed and tags" - {
        roundTrip(removeFormattingWhitespace("""
          <ul>
            <ul>
              <li piga="true" piga-collapsed="true" piga-tags="XX,YY">a</li>
              <ul>
                <ul>
                  <li piga="true">b</li>
                </ul>
              </ul>
            </ul>
            <li piga="true">c</li>
          </ul>
        """))
      }
    }
  }

  private def italic(string: String): TextWithMarkup = TextWithMarkup(string, Formatting(italic = true))

  private def removeFormattingWhitespace(s: String): String = {
    s.split('\n').map(_.trim).mkString("")
  }

  private class Module extends app.common.testing.TestModule {
    val desktopTaskEditor = new DesktopTaskEditor
  }
}
