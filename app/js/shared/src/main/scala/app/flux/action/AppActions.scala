package app.flux.action

import hydro.common.OrderToken
import app.models.document.DocumentEntity
import hydro.flux.action.Action

import scala.collection.immutable.Seq

object AppActions {

  // **************** Document-related actions **************** //
  case class AddEmptyDocument(name: String, orderToken: OrderToken) extends Action
  case class UpdateDocuments(documents: Seq[DocumentEntity]) extends Action
  case class RemoveDocument(existingDocument: DocumentEntity) extends Action
}
