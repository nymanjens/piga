package flux.react.app.desktop

import models.modification.EntityModification

import scala.collection.immutable.Seq

private[desktop] case class Task(id: Long, content: String)
private[desktop] object Task {
  def withRandomId(content: String): Task = Task(
    id = EntityModification.generateRandomId(),
    content = content
  )
}
