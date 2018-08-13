package flux.react.app.desktop

import api.ScalaJsApi.UserPrototype
import common.testing.Awaiter
import common.testing.TestObjects._
import flux.action.Action
import models.modification.EntityModification
import utest._

import scala.async.Async.{async, await}
import scala.collection.immutable.Seq
import scala.concurrent.duration._
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala2js.Converters._
import common.testing.JsTestObjects._
import flux.react.app.desktop.TaskSequence.{IndexedCursor, IndexedSelection}

object TaskEditorTest extends TestSuite {

  override def tests = TestSuite {
    val taskEditor = (new Module).taskEditor

    "convertToClipboardData" - {
      "covers multiple lines" - {
        taskEditor.convertToClipboardData(
          new TaskSequence(Seq(newTask("abc"), newTask("defg"), newTask("hij"))),
          IndexedSelection(start = IndexedCursor(0, 1), end = IndexedCursor(2, 2))) ==>
          taskEditor.ClipboardData(
            htmlText = "<ul><li>bc</li><li>defg</li><li>hi</li></ul>",
            plainText = "bc\ndefg\nhi")
      }
      "escapes html" - {
        taskEditor.convertToClipboardData(
          new TaskSequence(Seq(newTask("a<b>cd"))),
          IndexedSelection(start = IndexedCursor(0, 0), end = IndexedCursor(0, 5))) ==>
          taskEditor.ClipboardData(htmlText = "<ul><li>a&lt;b&gt;c</li></ul>", plainText = "a<b>c")
      }
      "converts newline to <br>" - {
        taskEditor.convertToClipboardData(
          new TaskSequence(Seq(newTask("a\nb"))),
          IndexedSelection(start = IndexedCursor(0, 0), end = IndexedCursor(0, 3))) ==>
          taskEditor.ClipboardData(htmlText = "<ul><li>a<br />b</li></ul>", plainText = "a\nb")
      }
      "handles indentation" - {
        taskEditor.convertToClipboardData(
          new TaskSequence(
            Seq(newTask("a", indentation = 2), newTask("b", indentation = 4), newTask("c", indentation = 1))),
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
      def replacementPart(content: String, indentation: Int = 0) =
        taskEditor.Replacement.Part(content, indentation)
      "without list tags" - {
        "with html" - {
          taskEditor.clipboardStringToReplacement(removeWhitespace("""
              <p>a<br />b</p>
              <div>c</div>
              d
            """)) ==>
            taskEditor.Replacement
              .create("a", replacementPart("b"), replacementPart("c"), replacementPart("d"))
        }
        "plain text" - {
          taskEditor.clipboardStringToReplacement("""
              |x
              |y
            """.stripMargin.trim) ==>
            taskEditor.Replacement.create("x", replacementPart("y"))
        }
      }
      "with list tags" - {
        "single level" - {
          taskEditor.clipboardStringToReplacement(removeWhitespace("""
             <ul>
               <li>
                 <p>a<br />b</p>
                 <div>c</div>
               </li>
               <li>xyz</li>
             </ul>
            """)) ==>
            taskEditor.Replacement.create("a\nb\nc", replacementPart("xyz"))
        }
      }
    }
    "convertToClipboardData(clipboardStringToReplacement)" - {
      def roundTrip(html: String): Unit = {
        val replacement = taskEditor.clipboardStringToReplacement(html)
        val clipboardData = taskEditor.convertToClipboardData(
          new TaskSequence(replacement.parts.map(p =>
            newTask(content = p.content, indentation = 10 + p.indentationRelativeToCurrent))),
          IndexedSelection(
            start = IndexedCursor(0, 0),
            end = IndexedCursor(replacement.parts.length - 1, replacement.parts.last.content.length))
        )
        clipboardData.htmlText ==> html
      }
      "covers multiple lines" - {
        roundTrip("<ul><li>bc</li><li>defg</li><li>hi</li></ul>")
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

  private def removeWhitespace(s: String): String = s.replace(" ", "").replace("\n", "")

  private def newTask(content: String, indentation: Int = 0): Task =
    Task.withRandomId(orderToken = orderTokenA, content = content, indentation = indentation)

  private class Module extends common.testing.TestModule {
    val taskEditor = new TaskEditor
  }
}
