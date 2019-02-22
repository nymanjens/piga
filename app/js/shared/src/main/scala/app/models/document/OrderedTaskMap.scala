package app.models.document

import hydro.common.OrderToken
import hydro.models.modification.EntityModification

import scala.collection.immutable.Seq
import scala.collection.mutable

final class OrderedTaskMap(val tasks: Seq[Task]) {

  def withAppliedEdit(documentEdit: DocumentEdit.WithUpdateTimes): OrderedTaskMap = {
    val toReplaceSet = toReplaceSeq.toSet
    val newTasks = mutable.Buffer[Task]()
    var toAddSeqIndex = 0
    def nextTaskToAdd: Option[Task] =
      if (toAddSeqIndex < toAddSeq.size) Some(toAddSeq(toAddSeqIndex)) else None

    for {
      t <- tasks
      if !toReplaceSet.contains(t)
    } {
      while (nextTaskToAdd.isDefined && nextTaskToAdd.get < t) {
        newTasks += nextTaskToAdd.get
        toAddSeqIndex += 1
      }
      if (nextTaskToAdd == Some(t)) {
        toAddSeqIndex += 1
      }
      newTasks += t
    }
    while (nextTaskToAdd.isDefined) {
      newTasks += nextTaskToAdd.get
      toAddSeqIndex += 1
    }

    new Document(id, name, orderToken, newTasks.toVector)
  }


}
