package app.models.document

import hydro.common.OrderToken
import hydro.models.modification.EntityType
import hydro.common.time.LocalDateTime
import hydro.models.Entity
import hydro.models.UpdatableEntity
import hydro.models.UpdatableEntity.LastUpdateTime

import java.time.Instant
import scala.collection.immutable.Seq

case class TaskEntity(
    documentId: Long,
    contentHtml: String,
    orderToken: OrderToken,
    indentation: Int,
    collapsed: Boolean,
    delayedUntil: Option[LocalDateTime],
    tags: Seq[String],
    lastContentModifierUserId: Long,
    override val idOption: Option[Long] = None,
    override val lastUpdateTime: LastUpdateTime = LastUpdateTime.neverUpdated,
) extends UpdatableEntity
    with Ordered[TaskEntity] {

  override def withId(id: Long) = copy(idOption = Some(id))
  override def withLastUpdateTime(time: LastUpdateTime): Entity = copy(lastUpdateTime = time)

  // **************** Ordered methods **************** //
  override def compare(that: TaskEntity): Int = {
    val result = this.orderToken compare that.orderToken
    if (result == 0)
      this.lastUpdateTime
        .mostRecentInstant()
        .getOrElse(Instant.EPOCH) compareTo that.lastUpdateTime.mostRecentInstant().getOrElse(Instant.EPOCH)
    else result
  }
}
object TaskEntity {
  implicit val Type: EntityType[TaskEntity] = EntityType()

  def tupled = (this.apply _).tupled
}
