package models.document

import common.OrderToken
import models.Entity

case class DocumentEntity(name: String, orderToken: OrderToken, idOption: Option[Long] = None)
    extends Entity {

  override def withId(id: Long) = copy(idOption = Some(id))

}
