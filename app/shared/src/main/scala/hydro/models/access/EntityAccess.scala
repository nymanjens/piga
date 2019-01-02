package hydro.models.access

import hydro.models.Entity
import app.models.modification.EntityType
import app.models.modification.EntityTypes
import app.models.document.TaskEntity
import app.models.document.DocumentEntity
import app.models.user.User
import app.models.user.User

/** Central point of access to the storage layer. */
trait EntityAccess {

  // **************** Getters ****************//
  def newQuery[E <: Entity: EntityType](): DbResultSet.Async[E]
}
