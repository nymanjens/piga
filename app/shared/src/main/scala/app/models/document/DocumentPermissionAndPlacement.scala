package app.models.document

import hydro.common.OrderToken
import hydro.models.Entity
import hydro.models.UpdatableEntity
import hydro.models.UpdatableEntity.LastUpdateTime
import hydro.models.modification.EntityType

/**
 * Combines a user and a document. The existence of this combination implies a permission. The extra data
 * indicates the placement of the document for that user.
 */
case class DocumentPermissionAndPlacement(
    documentId: Long,
    userId: Long,
    orderToken: OrderToken,
    override val idOption: Option[Long] = None,
    override val lastUpdateTime: LastUpdateTime = LastUpdateTime.neverUpdated,
) extends UpdatableEntity {

  override def withId(id: Long) = copy(idOption = Some(id))
  override def withLastUpdateTime(time: LastUpdateTime): Entity = copy(lastUpdateTime = time)
}
object DocumentPermissionAndPlacement {
  implicit val Type: EntityType[DocumentPermissionAndPlacement] = EntityType()

  def tupled = (this.apply _).tupled
}
