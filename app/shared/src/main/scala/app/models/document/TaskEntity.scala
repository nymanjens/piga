package app.models.document

import common.OrderToken
import common.time.LocalDateTime
import app.models.Entity

import scala.collection.immutable.Seq

case class TaskEntity(documentId: Long,
                      contentHtml: String,
                      orderToken: OrderToken,
                      indentation: Int,
                      collapsed: Boolean,
                      delayedUntil: Option[LocalDateTime],
                      tags: Seq[String],
                      idOption: Option[Long] = None)
    extends Entity {

  override def withId(id: Long) = copy(idOption = Some(id))
}
