package app.models.document

import hydro.common.OrderToken
import hydro.models.modification.EntityType
import hydro.models.Entity
import hydro.models.UpdatableEntity
import hydro.models.UpdatableEntity.LastUpdateTime

case class DocumentEntity(name: String,
                          orderToken: OrderToken,
                          override val idOption: Option[Long] = None,
                          override val lastUpdateTime: LastUpdateTime = LastUpdateTime.neverUpdated,
) extends UpdatableEntity
    with Ordered[DocumentEntity] {

  override def withId(id: Long) = copy(idOption = Some(id))
  override def withLastUpdateTime(time: LastUpdateTime): Entity = copy(lastUpdateTime = time)

  // **************** Ordered methods **************** //
  override def compare(that: DocumentEntity): Int = {
    this.orderToken compare that.orderToken
  }
}
object DocumentEntity {
  implicit val Type: EntityType[DocumentEntity] = EntityType()

  def tupled = (this.apply _).tupled
}
