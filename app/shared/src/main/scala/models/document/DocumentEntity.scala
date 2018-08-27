package models.document

import models.Entity

case class DocumentEntity(name: String, idOption: Option[Long] = None) extends Entity {

  override def withId(id: Long) = copy(idOption = Some(id))

}
