package app.flux.react.app.document

import app.models.document.Task

import scala.collection.immutable.Seq

private[document] object TaskEditorUtils {

  case class TaskInSeq(
      task: Task,
      index: Int,
      maybeAmountCollapsed: Option[Int],
      isRoot: Boolean,
      isLeaf: Boolean,
  )

  def applyCollapsedProperty(tasks: Seq[Task]): Stream[TaskInSeq] = {
    def getAmountCollapsed(tasksWithIndex: Stream[(Task, Int)], collapsedIndentation: Int): Int = {
      tasksWithIndex.takeWhile(_._1.indentation > collapsedIndentation).size
    }

    def inner(tasksWithIndex: Stream[(Task, Int)]): Stream[TaskInSeq] = tasksWithIndex match {
      case (task, index) #:: rest =>
        val maybeAmountCollapsed =
          if (task.collapsed) Some(getAmountCollapsed(rest, task.indentation)) else None

        val isRoot = task.indentation == 0
        val isLeaf = getOption(tasks, index = index + 1) match {
          case Some(nextTask) if nextTask.indentation > task.indentation => false
          case _                                                         => true
        }

        val nextTaskInSeq = TaskInSeq(
          task = task,
          index = index,
          maybeAmountCollapsed = maybeAmountCollapsed,
          isRoot = isRoot,
          isLeaf = isLeaf)
        nextTaskInSeq #:: inner(rest.drop(maybeAmountCollapsed getOrElse 0))
      case Stream.Empty => Stream.Empty
    }

    inner(tasks.toStream.zipWithIndex)
  }

  private def getOption[T](seq: Seq[T], index: Int): Option[T] =
    if (seq.indices.contains(index)) Some(seq(index)) else None
}
