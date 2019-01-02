package app.models.modification

import app.models.document.DocumentEntity
import app.models.document.TaskEntity
import app.models.user.User

import scala.collection.immutable.Seq

object EntityTypes {

  lazy val all: Seq[EntityType.any] = Seq(User.Type, DocumentEntity.Type, TaskEntity.Type)
}
