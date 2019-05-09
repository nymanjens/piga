package app.flux.react.app.document

import app.common.testing.JsTestObjects._
import app.flux.react.app.document.TaskEditorUtils.TaskInSeq
import utest._

import scala.collection.immutable.Seq

object TaskEditorUtilsTest extends TestSuite {

  override def tests = TestSuite {

    "applyCollapsedProperty" - {
      "empty Seq" - {
        TaskEditorUtils.applyCollapsedProperty(Seq()) ==> Stream.empty
      }
      "no collapsed tasks" - {
        val task1 = taskA.copyForTests(indentation = 0)
        val task2 = taskB.copyForTests(indentation = 2)
        val task3 = taskC.copyForTests(indentation = 0)
        TaskEditorUtils.applyCollapsedProperty(Seq(task1, task2, task3)).toVector ==> Seq(
          TaskInSeq(task1, index = 0, maybeAmountCollapsed = None, isRoot = true, isLeaf = false),
          TaskInSeq(task2, index = 1, maybeAmountCollapsed = None, isRoot = false, isLeaf = true),
          TaskInSeq(task3, index = 2, maybeAmountCollapsed = None, isRoot = true, isLeaf = true),
        )
      }
      "collapsed tasks" - {
        val task1 = taskA.copyForTests(indentation = 0, collapsed = true)
        val task2 = taskB.copyForTests(indentation = 0, collapsed = true)
        val task3 = taskC.copyForTests(indentation = 2)
        val task4 = taskD.copyForTests(indentation = 0)
        TaskEditorUtils.applyCollapsedProperty(Seq(task1, task2, task3, task4)).toVector ==> Seq(
          TaskInSeq(task1, index = 0, maybeAmountCollapsed = Some(0), isRoot = true, isLeaf = true),
          TaskInSeq(task2, index = 1, maybeAmountCollapsed = Some(1), isRoot = true, isLeaf = false),
          TaskInSeq(task4, index = 3, maybeAmountCollapsed = None, isRoot = true, isLeaf = true),
        )
      }
    }
  }
}
