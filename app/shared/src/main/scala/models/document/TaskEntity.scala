package models.document

import common.OrderToken
import models.Entity

case class TaskEntity(documentId: Long,
                      contentHtml: String,
                      orderToken: OrderToken,
                      indentation: Int,
                      idOption: Option[Long] = None)
    extends Entity {

  override def withId(id: Long) = copy(idOption = Some(id))
}
