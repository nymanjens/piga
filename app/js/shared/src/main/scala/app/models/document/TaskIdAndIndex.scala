package app.models.document

import app.models.document.Document.IndexedCursor

case class TaskIdAndIndex(taskId: Long, taskIndex: Int) {
  def inDocument(document: Document): TaskIdAndIndex = {
    if (document.tasksOption(taskIndex).exists(_.id == taskId)) {
      // This instance is consistent with the given document
      this
    } else {
      document.maybeIndexOf(taskId) match {
        // The taskIndex was updated, but the taskId is still present --> follow the existing taskId
        case Some(newTaskIndex) => TaskIdAndIndex(taskId = taskId, taskIndex = newTaskIndex)
        case None =>
          document.tasksOption(taskIndex) match {
            case Some(newTask) =>
              // The task no longer exists so fall back to the taskIndex
              TaskIdAndIndex(taskId = newTask.id, taskIndex = taskIndex)
            case None =>
              TaskIdAndIndex(taskId = taskId, taskIndex = 0)
          }
      }
    }
  }

}
object TaskIdAndIndex {
  val nullInstance: TaskIdAndIndex = TaskIdAndIndex(taskId = -1, taskIndex = 0)

  def fromIndexedCursor(indexedCursor: IndexedCursor)(implicit document: Document): TaskIdAndIndex = {
    fromTaskIndex(indexedCursor.seqIndex)
  }
  def fromTaskIndex(seqIndex: Int)(implicit document: Document): TaskIdAndIndex = {
    TaskIdAndIndex(
      taskId = document.tasks(seqIndex).id,
      taskIndex = seqIndex,
    )
  }
}
