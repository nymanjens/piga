package app.models.document

import app.common.OrderToken
import hydro.models.modification.EntityType
import hydro.models.Entity

case class DocumentEntity(name: String, orderToken: OrderToken, idOption: Option[Long] = None)
    extends Entity
    with Ordered[DocumentEntity] {

  override def withId(id: Long) = copy(idOption = Some(id))

  // **************** Ordered methods **************** //
  override def compare(that: DocumentEntity): Int = {
    this.orderToken compare that.orderToken
  }
}
object DocumentEntity {
  implicit val Type: EntityType[DocumentEntity] = EntityType()

  def tupled = (this.apply _).tupled
}
