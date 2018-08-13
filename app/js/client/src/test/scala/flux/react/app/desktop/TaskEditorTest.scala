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
            htmlText = """
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
            """.replace(" ", "").replace("\n", ""),
            plainText = "a\nb\nc"
          )
      }
    }
    "clipboardStringToReplacement" - {
      //clipboardStringToReplacement
    }
  }

  private def newTask(content: String, indentation: Int = 0): Task =
    Task.withRandomId(orderToken = orderTokenA, content = content, indentation = indentation)

  private class Module extends common.testing.TestModule {
    val taskEditor = new TaskEditor
  }
}
