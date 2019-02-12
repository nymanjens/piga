package app.models.document

import hydro.common.OrderToken
import hydro.models.modification.EntityType
import hydro.common.time.LocalDateTime
import hydro.models.Entity
import hydro.models.UpdatableEntity
import hydro.models.UpdatableEntity.LastUpdateTime

import scala.collection.immutable.Seq

case class TaskEntity(documentId: Long,
                      contentHtml: String,
                      orderToken: OrderToken,
                      indentation: Int,
                      collapsed: Boolean,
                      delayedUntil: Option[LocalDateTime],
                      tags: Seq[String],
                      override val idOption: Option[Long] = None,
                      override val lastUpdateTime: LastUpdateTime = LastUpdateTime.neverUpdated,
) extends UpdatableEntity {

  override def withId(id: Long) = copy(idOption = Some(id))
  override def withLastUpdateTime(time: LastUpdateTime): Entity = copy(lastUpdateTime = time)
}
object TaskEntity {
  implicit val Type: EntityType[TaskEntity] = EntityType()

  def tupled = (this.apply _).tupled
}
