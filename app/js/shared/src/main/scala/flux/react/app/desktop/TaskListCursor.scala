package flux.react.app.desktop

import flux.react.app.desktop.DomWalker.NodeWithOffset
import org.scalajs.dom

import scala.annotation.tailrec
import scala.collection.immutable.Seq

private[desktop] case class TaskListCursor(listIndex: Int, offsetInTask: Int)
    extends Ordered[TaskListCursor] {

  override def compare(that: TaskListCursor): Int = {
    import scala.math.Ordered.orderingToOrdered
    (this.listIndex, this.offsetInTask) compare ((that.listIndex, that.offsetInTask))
  }

  def plusOffset(diff: Int): TaskListCursor = TaskListCursor(listIndex, offsetInTask + diff)
  def minusOffset(diff: Int): TaskListCursor = plusOffset(-diff)

  def plusOffsetInList(diff: Int)(implicit tasks: Seq[Task]): TaskListCursor = {
    @tailrec
    def fixOffset(c: TaskListCursor): TaskListCursor = c.offsetInTask match {
      case offset if offset < 0 =>
        if (c.listIndex == 0) {
          TaskListCursor(0, 0)
        } else {
          fixOffset(TaskListCursor(c.listIndex - 1, tasks(c.listIndex - 1).content.length + offset + 1))
        }
      case offset if offset > tasks(c.listIndex).content.length =>
        if (c.listIndex == tasks.length - 1) {
          TaskListCursor(tasks.length - 1, tasks.last.content.length)
        } else {
          fixOffset(TaskListCursor(c.listIndex + 1, offset - tasks(c.listIndex).content.length - 1))
        }
      case _ => c
    }
    fixOffset(TaskListCursor(listIndex, offsetInTask + diff))
  }
  def minusOffsetInList(diff: Int)(implicit tasks: Seq[Task]): TaskListCursor = plusOffsetInList(-diff)

  def plusWord(implicit tasks: Seq[Task]): TaskListCursor = moveWord(step = 1)
  def minusWord(implicit tasks: Seq[Task]): TaskListCursor = moveWord(step = -1)
  private def moveWord(step: Int)(implicit tasks: Seq[Task]): TaskListCursor = {
    val result = copy(offsetInTask = {
      val task = tasks(listIndex).content
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
      plusOffsetInList(step)
    } else {
      result
    }
  }
}
private object TaskListCursor {
  def tupleFromSelection(selection: dom.raw.Selection): (TaskListCursor, TaskListCursor) = {
    val anchor = TaskListCursor.fromNode(selection.anchorNode, selection.anchorOffset)
    val focus = TaskListCursor.fromNode(selection.focusNode, selection.focusOffset)
    if (anchor < focus) (anchor, focus) else (focus, anchor)
  }

  def fromNode(node: dom.raw.Node, offset: Int): TaskListCursor = {
    val parentLi = parentLiElement(node)
    val nodeWithOffset = DomWalker.depthFirstPreOrder(parentLi).find(_.node isEqualNode node).get
    TaskListCursor(
      listIndex = parentLi.getAttribute("num").toInt,
      offsetInTask = nodeWithOffset.offsetSoFar + offset)
  }

  private def parentLiElement(node: dom.raw.Node): dom.raw.Element = {
    if (node.nodeType == dom.raw.Node.ELEMENT_NODE && node.asInstanceOf[dom.raw.Element].tagName == "LI") {
      node.asInstanceOf[dom.raw.Element]
    } else {
      parentLiElement(node.parentNode)
    }
  }
}
