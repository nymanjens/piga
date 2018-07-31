package flux.react.app.desktop

import scala.collection.immutable.Seq

private[desktop] final class TaskSequence(tasks: Seq[Task]) {
  require(tasks.sorted == tasks, tasks) // TODD: Remove this check when we're confident that this works

  def option(index: Int): Option[Task] = index match {
    case i if i < 0             => None
    case i if i >= tasks.length => None
    case _                      => Some(tasks(index))
  }

  // **************** Methods that delegate to Seq[Task] **************** //
  def length: Int = tasks.length
  def zipWithIndex: Seq[(Task, Int)] = tasks.zipWithIndex
  def apply(index: Int): Task = tasks(index)
  def updated(index: Int, task: Task): TaskSequence = new TaskSequence(tasks.updated(index, task))
  def last: Task = tasks.last
}
