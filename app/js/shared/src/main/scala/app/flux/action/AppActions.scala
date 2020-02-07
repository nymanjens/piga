package app.flux.action

import app.common.document.UserDocument
import hydro.common.OrderToken
import app.models.document.DocumentEntity
import hydro.flux.action.Action

import scala.collection.immutable.Seq

object AppActions {

  // **************** Document-related actions **************** //
  case class AddEmptyDocument(name: String, orderToken: OrderToken) extends Action
  case class UpdateDocuments(documents: Seq[UserDocument]) extends Action
  case class RemoveDocument(existingDocument: UserDocument) extends Action
}
