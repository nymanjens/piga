package app.common.document

import app.models.access.ModelFields
import app.models.document.DocumentEntity
import app.models.document.DocumentPermissionAndPlacement
import app.models.user.User
import hydro.common.OrderToken
import hydro.models.access.DbQueryImplicits._
import hydro.models.access.EntityAccess

import scala.async.Async.async
import scala.async.Async.await
import scala.collection.immutable.Seq
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

// Note: The user is assumed to be the implicit user
case class UserDocument(
    documentId: Long,
    name: String,
    orderToken: OrderToken,
)

object UserDocument {

  def fetchAllForUser()(implicit user: User, entityAccess: EntityAccess): Future[Seq[UserDocument]] = async {
    val permissionAndPlacements = await(
      entityAccess
        .newQuery[DocumentPermissionAndPlacement]()
        .filter(ModelFields.DocumentPermissionAndPlacement.userId === user.id)
        .data()
    )

    val documentEntities =
      await(
        entityAccess
          .newQuery[DocumentEntity]()
          .filter(ModelFields.DocumentEntity.id isAnyOf (permissionAndPlacements.map(_.documentId)))
          .data()
      )
    val documentEntityMap = uniqueIndex(documentEntities)(_.id)

    val unsortedResult =
      for (permissionAndPlacement <- permissionAndPlacements) yield {
        val documentEntity = documentEntityMap(permissionAndPlacement.documentId)
        UserDocument(
          documentId = documentEntity.id,
          name = documentEntity.name,
          orderToken = permissionAndPlacement.orderToken,
        )
      }

    unsortedResult.sortBy(_.orderToken)
  }

  private def uniqueIndex[K, V](iterable: Iterable[V])(keyMapper: V => K): Map[K, V] = {
    iterable.map(v => (keyMapper(v) -> v)).toMap
  }
}
