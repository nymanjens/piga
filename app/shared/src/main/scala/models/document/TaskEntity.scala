package models.document

import common.OrderToken
import models.Entity

case class TaskEntity(orderToken: OrderToken, contentHtml: String, indentation: Int, idOption: Option[Long])
    extends Entity {

  override def withId(id: Long) = copy(idOption = Some(id))
}
