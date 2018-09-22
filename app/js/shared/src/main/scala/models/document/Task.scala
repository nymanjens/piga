package models.document

import common.OrderToken
import models.modification.EntityModification

final class Task private (val id: Long,
                          val content: TextWithMarkup,
                          val orderToken: OrderToken,
                          val indentation: Int)
    extends Ordered[Task] {

  def contentString: String = content.contentString

  def equalsIgnoringId(that: Task): Boolean = {
    this.content == that.content && this.orderToken == that.orderToken && this.indentation == that.indentation
  }

  def toTaskEntity(implicit document: Document): TaskEntity =
    TaskEntity(
      documentId = document.id,
      contentHtml = content.toHtml,
      orderToken = orderToken,
      indentation = indentation,
      idOption = Some(id))

  def copyWithId(newId: Long): Task =
    new Task(id = newId, content = content, orderToken = orderToken, indentation = indentation)

  def copyWithRandomId(content: TextWithMarkup = null,
                       orderToken: OrderToken = null,
                       indentation: Int = -1): Task = {
    Task.withRandomId(
      content = Option(content) getOrElse this.content,
      orderToken = Option(orderToken) getOrElse this.orderToken,
      indentation = if (indentation == -1) this.indentation else indentation
    )
  }

  // **************** Ordered methods **************** //
  override def compare(that: Task): Int = {
    this.orderToken compare that.orderToken
  }

  // **************** Object methods **************** //
  override def toString: String = s"Task($id, $contentString)"
  override def equals(o: scala.Any): Boolean = o match {
    case that: Task => this.id == that.id
    case _          => false
  }
  override def hashCode(): Int = id.hashCode()
}
object Task {
  def withRandomId(content: TextWithMarkup, orderToken: OrderToken, indentation: Int): Task =
    new Task(
      id = EntityModification.generateRandomId(),
      content = content,
      orderToken = orderToken,
      indentation = indentation
    )

  def fromTaskEntity(taskEntity: TaskEntity): Task =
    new Task(
      id = taskEntity.id,
      content = TextWithMarkup.fromHtml(taskEntity.contentHtml),
      orderToken = taskEntity.orderToken,
      indentation = taskEntity.indentation)
}
