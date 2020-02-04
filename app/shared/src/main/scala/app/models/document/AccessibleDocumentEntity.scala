package app.models.document

import hydro.common.OrderToken
import hydro.models.Entity
import hydro.models.UpdatableEntity
import hydro.models.UpdatableEntity.LastUpdateTime

case class AccessibleDocumentEntity(
    documentId: Long,
    userId: Long,
    orderToken: OrderToken,
    override val idOption: Option[Long] = None,
    override val lastUpdateTime: LastUpdateTime = LastUpdateTime.neverUpdated,
) extends UpdatableEntity
    with Ordered[AccessibleDocumentEntity] {

  override def withId(id: Long) = copy(idOption = Some(id))
  override def withLastUpdateTime(time: LastUpdateTime): Entity = copy(lastUpdateTime = time)

  // **************** Ordered methods **************** //
  override def compare(that: AccessibleDocumentEntity): Int = {
    this.orderToken compare that.orderToken
  }
}
