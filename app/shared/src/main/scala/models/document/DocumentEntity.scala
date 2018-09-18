package models.document

import common.OrderToken
import models.Entity

case class DocumentEntity(name: String, orderToken: OrderToken, idOption: Option[Long] = None)
    extends Entity
    with Ordered[DocumentEntity] {

  override def withId(id: Long) = copy(idOption = Some(id))

  // **************** Ordered methods **************** //
  override def compare(that: DocumentEntity): Int = {
    this.orderToken compare that.orderToken
  }
}
