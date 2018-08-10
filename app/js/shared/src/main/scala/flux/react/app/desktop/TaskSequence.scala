package flux.react.app.desktop

import scala.collection.immutable.Seq

private[desktop] final class TaskSequence(tasks: Seq[Task]) {
  require(tasks.sorted == tasks, tasks) // TODD: Remove this check when we're confident that this works

  def replaced(toReplace: Iterable[Task], toAdd: Iterable[Task]): TaskSequence =
    (toReplace.toVector, toAdd.toVector) match {
      case (Seq(replace), Seq(add)) if replace.orderToken == add.orderToken =>
        // Optimization
        val taskIndex = indexOf(replace)
        new TaskSequence(tasks.updated(taskIndex, add))

      case (toReplaceSeq, toAddSeq) =>
        val toReplaceSet = toReplaceSeq.toSet
        new TaskSequence(tasks.flatMap {
          case task if task == toReplaceSeq.head  => toAddSeq
          case task if toReplaceSet contains task => Seq()
          case task                               => Seq(task)
        })
    }

  def option(index: Int): Option[Task] = index match {
    case i if i < 0             => None
    case i if i >= tasks.length => None
    case _                      => Some(tasks(index))
  }

  // **************** Methods that delegate to Seq[Task] **************** //
  def zipWithIndex: Seq[(Task, Int)] = tasks.zipWithIndex
  def length: Int = tasks.length

  def apply(index: Int): Task = tasks(index)

  // **************** Private methods **************** //
  def indexOf(task: Task): Int = {
    def inner(lowerIndex: Int, upperIndex: Int): Int = {
      require(lowerIndex <= upperIndex, s"$task is not in $tasks")
      val index = (upperIndex + lowerIndex) / 2
      tasks(index) match {
        case t if t.id == task.id =>
          require(task == t)
          index
        case t if task.orderToken < t.orderToken => inner(lowerIndex, upperIndex - 1)
        case _                                   => inner(index + 1, upperIndex)
      }
    }

    inner(0, tasks.length - 1)
  }
}
