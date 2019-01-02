package app.models.document

import app.common.OrderToken
import app.models.modification.EntityType
import app.models.modification.EntityTypes
import hydro.common.time.LocalDateTime
import hydro.models.Entity

import scala.collection.immutable.Seq

case class TaskEntity(documentId: Long,
                      contentHtml: String,
                      orderToken: OrderToken,
                      indentation: Int,
                      collapsed: Boolean,
                      delayedUntil: Option[LocalDateTime],
                      tags: Seq[String],
                      idOption: Option[Long] = None)
    extends Entity {

  override def withId(id: Long) = copy(idOption = Some(id))
}
object TaskEntity {
  implicit val Type: EntityType[TaskEntity] = EntityType()

  def tupled = (this.apply _).tupled
}
