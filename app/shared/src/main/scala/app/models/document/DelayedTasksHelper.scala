package app.models.document

import hydro.common.time.JavaTimeImplicits._
import app.models.document.DelayedTasksHelper.TaskUpdateCreator
import hydro.common.time.LocalDateTime
import hydro.common.CollectionUtils
import hydro.common.OrderToken

import java.time.LocalDate
import scala.collection.immutable.Seq

case class DelayedTasksHelper[TaskT, UpdateT](
    tasks: Seq[TaskT],
    taskIndentation: TaskT => Int,
    taskDelayedUntil: TaskT => Option[LocalDateTime],
    taskTags: TaskT => Seq[String],
    taskOrderToken: TaskT => OrderToken,
    taskCollapsed: TaskT => Boolean,
    taskUpdateCreator: TaskUpdateCreator[TaskT, UpdateT],
) {

  def containsDelayedTasks(): Boolean = {
    tasks.exists(_.delayedUntil.isDefined)
  }

  def missingTags(): Seq[String] = {
    Seq("#todo_unsorted", "#delayed_tasks").filter(tag => !tasks.exists(_.tags.contains(tag)))
  }

  lazy val delayedRootTasks: Seq[TaskT] = {
    delayedTasksIndices.map(tasks).filter { t =>
      t.delayedUntil.isDefined && t.indentation == delayedTasksParent.indentation + 1
    }
  }

  def validateTasks(): Unit = {
    def isInReverseOrder(datetimes: Seq[LocalDate]): Boolean = {
      datetimes.sliding(2).forall {
        case Seq(a, b) => a >= b
        case _         => true // Handles lists with fewer than 2 elements
      }
    }
    if (containsDelayedTasks()) {
      require(maybeTodoUnsortedParent.isDefined)
      require(maybeDelayedTasksParent.isDefined)
      for ((task, taskIndex) <- tasks.zipWithIndex) {
        if (task.delayedUntil.isDefined) {
          require(delayedTasksIndices.contains(taskIndex), task)
          require(task.indentation == delayedTasksParent.indentation + 1, task)
        }
      }
      for (taskIndex <- delayedTasksIndices) {
        val task = tasks(taskIndex)
        require(task.indentation == delayedTasksParent.indentation + 1 == task.delayedUntil.isDefined, task)
      }

      require(
        isInReverseOrder(delayedRootTasks.map(_.delayedUntil.get.toLocalDate)),
        s"Delayed tasks in the wrong order: $delayedRootTasks",
      )
    }
  }

  def toTodoUnsorted(rootTasks: Seq[TaskT]): Seq[UpdateT] = {
    val todoUnsortedChildIndices = indicesIncludingChildren(todoUnsortedParent)

    // Gather all tasks to move
    val tasksToMove = rootTasks.flatMap(t => indicesIncludingChildren(t)).map(tasks)

    // Generate new OrderTokens for them, appending to #todo_unsorted
    val newOrderTokens = OrderToken.evenlyDistributedValuesBetween(
      numValues = tasksToMove.size,
      lowerExclusive = Some(tasks(todoUnsortedChildIndices.max).orderToken),
      higherExclusive = CollectionUtils.maybeGet(tasks, todoUnsortedChildIndices.max + 1).map(_.orderToken),
    )

    for ((task, newOrderToken) <- tasksToMove zip newOrderTokens) yield {
      taskUpdateCreator.createUpdate(
        task,
        orderToken = newOrderToken,
        indentation = task.indentation - delayedTasksParent.indentation + todoUnsortedParent.indentation,
        delayedUntil = None,
        collapsed = task.collapsed,
      )
    }
  }

  def toDelayedTasks(rootTask: TaskT, delayedUntil: LocalDateTime): Seq[UpdateT] = {
    val tasksToMove = indicesIncludingChildren(rootTask).map(tasks)
    val maybeNextRootTask = delayedRootTasks.find(_.delayedUntil.get.toLocalDate <= delayedUntil.toLocalDate)
    val previousTaskIndex = maybeNextRootTask match {
      case Some(task) => tasks.indexOf(task) - 1
      case None       => indicesIncludingChildren(delayedTasksParent).max // Insert at the end of the list
    }
    val newOrderTokens = OrderToken.evenlyDistributedValuesBetween(
      numValues = tasksToMove.size,
      lowerExclusive = Some(tasks(previousTaskIndex).orderToken),
      higherExclusive = CollectionUtils.maybeGet(tasks, previousTaskIndex + 1).map(_.orderToken),
    )

    for ((task, newOrderToken) <- tasksToMove zip newOrderTokens)
      yield {
        taskUpdateCreator.createUpdate(
          task = task,
          orderToken = newOrderToken,
          indentation = task.indentation - rootTask.indentation + delayedTasksParent.indentation + 1,
          delayedUntil = if (task == rootTask) Some(delayedUntil) else None,
          collapsed = if (task == rootTask) tasksToMove.size > 1 else task.collapsed,
        )
      }
  }

  private lazy val maybeTodoUnsortedParent: Option[TaskT] = tasks.find(_.tags.contains("#todo_unsorted"))
  private lazy val maybeDelayedTasksParent: Option[TaskT] = tasks.find(_.tags.contains("#delayed_tasks"))
  private lazy val todoUnsortedParent: TaskT = maybeTodoUnsortedParent.get
  private lazy val delayedTasksParent: TaskT = maybeDelayedTasksParent.get

  private lazy val delayedTasksIndices: Range = indicesIncludingChildren(delayedTasksParent).drop(1)

  private def indicesIncludingChildren(task: TaskT): Range = {
    val endIndex =
      tasks.indexWhere(_.indentation <= task.indentation, from = tasks.indexOf(task) + 1) match {
        case -1 => tasks.size
        case i  => i
      }
    tasks.indexOf(task) until endIndex
  }

  implicit private class TaskWrapper(task: TaskT) {
    def indentation: Int = taskIndentation(task)
    def delayedUntil: Option[LocalDateTime] = taskDelayedUntil(task)
    def tags: Seq[String] = taskTags(task)
    def orderToken: OrderToken = taskOrderToken(task)
    def collapsed: Boolean = taskCollapsed(task)
  }
}
object DelayedTasksHelper {
  trait TaskUpdateCreator[TaskT, UpdateT] {
    def createUpdate(
        task: TaskT,
        orderToken: OrderToken,
        indentation: Int,
        delayedUntil: Option[LocalDateTime],
        collapsed: Boolean,
    ): UpdateT
  }
}
