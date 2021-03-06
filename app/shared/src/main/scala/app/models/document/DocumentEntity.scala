package app.models.document

import hydro.common.OrderToken
import hydro.models.modification.EntityType
import hydro.models.Entity
import hydro.models.UpdatableEntity
import hydro.models.UpdatableEntity.LastUpdateTime

case class DocumentEntity(
    name: String,
    override val idOption: Option[Long] = None,
    override val lastUpdateTime: LastUpdateTime = LastUpdateTime.neverUpdated,
) extends UpdatableEntity {

  override def withId(id: Long) = copy(idOption = Some(id))
  override def withLastUpdateTime(time: LastUpdateTime): Entity = copy(lastUpdateTime = time)
}
object DocumentEntity {
  implicit val Type: EntityType[DocumentEntity] = EntityType()

  def tupled = (this.apply _).tupled
}
