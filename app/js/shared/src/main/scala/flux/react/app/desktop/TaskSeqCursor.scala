package flux.react.app.desktop

import common.DomNodeUtils.nodeIsLi
import org.scalajs.dom

import scala.annotation.tailrec

private[desktop] case class TaskSeqCursor(seqIndex: Int, offsetInTask: Int) extends Ordered[TaskSeqCursor] {

  override def compare(that: TaskSeqCursor): Int = {
    import scala.math.Ordered.orderingToOrdered
    (this.seqIndex, this.offsetInTask) compare ((that.seqIndex, that.offsetInTask))
  }

  def proceedNTasks(n: Int): TaskSeqCursor = n match {
    case 0 => this
    case _ => TaskSeqCursor(seqIndex + n, 0)
  }
  def plusOffset(diff: Int): TaskSeqCursor = TaskSeqCursor(seqIndex, offsetInTask + diff)
  def minusOffset(diff: Int): TaskSeqCursor = plusOffset(-diff)

  def plusOffsetInSeq(diff: Int)(implicit tasks: TaskSequence): TaskSeqCursor = {
    @tailrec
    def fixOffset(c: TaskSeqCursor): TaskSeqCursor = c.offsetInTask match {
      case offset if offset < 0 =>
        if (c.seqIndex == 0) {
          TaskSeqCursor(0, 0)
        } else {
          fixOffset(TaskSeqCursor(c.seqIndex - 1, tasks(c.seqIndex - 1).content.length + offset + 1))
        }
      case offset if offset > tasks(c.seqIndex).content.length =>
        if (c.seqIndex == tasks.length - 1) {
          TaskSeqCursor(tasks.length - 1, tasks(tasks.length - 1).content.length)
        } else {
          fixOffset(TaskSeqCursor(c.seqIndex + 1, offset - tasks(c.seqIndex).content.length - 1))
        }
      case _ => c
    }
    fixOffset(TaskSeqCursor(seqIndex, offsetInTask + diff))
  }
  def minusOffsetInSeq(diff: Int)(implicit tasks: TaskSequence): TaskSeqCursor = plusOffsetInSeq(-diff)

  def plusWord(implicit tasks: TaskSequence): TaskSeqCursor = moveWord(step = 1)
  def minusWord(implicit tasks: TaskSequence): TaskSeqCursor = moveWord(step = -1)
  private def moveWord(step: Int)(implicit tasks: TaskSequence): TaskSeqCursor = {
    val result = copy(offsetInTask = {
      val task = tasks(seqIndex).content
      @tailrec
      def move(offsetInTask: Int, seenWord: Boolean = false): Int = {
        val nextOffset = offsetInTask + step
        if (nextOffset < 0 || nextOffset > task.length) {
          offsetInTask
        } else {
          val currentChar = if (step > 0) task.charAt(offsetInTask) else task.charAt(nextOffset)
          val currentCharIsWord = currentChar.isLetterOrDigit
          if (currentCharIsWord) {
            move(nextOffset, seenWord = true)
          } else {
            if (seenWord) {
              offsetInTask
            } else {
              move(nextOffset, seenWord = false)
            }
          }
        }
      }
      move(offsetInTask)
    })
    if (this == result) {
      // No movement happened --> move to the next/previous line
      plusOffsetInSeq(step)
    } else {
      result
    }
  }
}
private object TaskSeqCursor {
  def tupleFromSelection(selection: dom.raw.Selection): (TaskSeqCursor, TaskSeqCursor) = {
    val anchor = TaskSeqCursor.fromNode(selection.anchorNode, selection.anchorOffset)
    val focus = TaskSeqCursor.fromNode(selection.focusNode, selection.focusOffset)
    if (anchor < focus) (anchor, focus) else (focus, anchor)
  }

  def fromNode(node: dom.raw.Node, offset: Int): TaskSeqCursor = {
    val parentLi = parentLiElement(node)

    val offsetInTask = {
      val preCursorRange = dom.document.createRange()
      preCursorRange.selectNodeContents(parentLi)
      preCursorRange.setEnd(node, offset)
      preCursorRange.toString.length
    }

    TaskSeqCursor(seqIndex = parentLi.getAttribute("num").toInt, offsetInTask = offsetInTask)
  }

  private def parentLiElement(node: dom.raw.Node): dom.raw.Element = {
    if (nodeIsLi(node)) {
      node.asInstanceOf[dom.raw.Element]
    } else {
      parentLiElement(node.parentNode)
    }
  }
}
