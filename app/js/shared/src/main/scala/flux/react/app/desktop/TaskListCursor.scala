package flux.react.app.desktop

import org.scalajs.dom

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
}
private object TaskListCursor {
  def tupleFromSelection(selection: dom.raw.Selection): (TaskListCursor, TaskListCursor) = {
    val anchor = TaskListCursor.fromNode(selection.anchorNode, selection.anchorOffset)
    val focus = TaskListCursor.fromNode(selection.focusNode, selection.focusOffset)
    if (anchor < focus) (anchor, focus) else (focus, anchor)
  }

  def fromNode(node: dom.raw.Node, offset: Int): TaskListCursor =
    TaskListCursor(listIndex = parentElement(node).getAttribute("num").toInt, offsetInTask = offset)

  private def parentElement(node: dom.raw.Node): dom.raw.Element = {
    if (node.nodeType == dom.raw.Node.ELEMENT_NODE) {
      node.asInstanceOf[dom.raw.Element]
    } else {
      parentElement(node.parentNode)
    }
  }
}
