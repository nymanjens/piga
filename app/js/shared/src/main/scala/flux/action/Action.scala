package flux.action

import api.ScalaJsApi.UserPrototype
import models.document.DocumentEntity

import scala.collection.immutable.Seq

sealed trait Action

object Action {

  // **************** User-related actions **************** //
  case class UpsertUser(userPrototype: UserPrototype) extends Action

  // **************** Document-related actions **************** //
  case class AddDocument(documentWithoutId: DocumentEntity) extends Action
  case class UpdateDocuments(newDocuments: Seq[DocumentEntity]) extends Action
  case class RemoveDocument(existingDocument: DocumentEntity) extends Action

  // **************** Other actions **************** //
  case class SetPageLoadingState(isLoading: Boolean) extends Action

  /** Special action that gets sent to the dispatcher's callbacks after they processed the contained action. */
  case class Done private[action] (action: Action) extends Action

  /** Special action that gets sent to the dispatcher's callbacks after processing an action failed. */
  case class Failed private[action] (action: Action) extends Action
}
