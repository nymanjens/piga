package app.common.document

import scala.collection.immutable.Seq

import app.models.access.ModelFields
import app.models.document.DocumentEntity

import scala.concurrent.ExecutionContext.Implicits.global
import scala.async.Async.async
import scala.async.Async.await
import hydro.models.access.DbQueryImplicits._
import app.models.document.DocumentPermissionAndPlacement
import app.models.user.User
import hydro.common.GuavaReplacement
import hydro.common.OrderToken
import hydro.models.access.EntityAccess

import scala.concurrent.Future

case class UserDocument(
    documentId: Long,
    name: String,
    orderToken: OrderToken,
) extends Ordered[UserDocument] {

  override def compare(that: UserDocument): Int = {
    this.orderToken compare that.orderToken
  }
}

object UserDocument {
  def fetchAllForUser()(implicit user: User, entityAccess: EntityAccess): Future[Seq[UserDocument]] = async {
    val permissionAndPlacements = await(
      entityAccess
        .newQuery[DocumentPermissionAndPlacement]()
        .filter(ModelFields.DocumentPermissionAndPlacement.userId === user.id)
        .data())

    val documentEntities =
      await(
        entityAccess
          .newQuery[DocumentEntity]()
          .filter(ModelFields.DocumentEntity.id isAnyOf (permissionAndPlacements.map(_.documentId)))
          .data())
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

    unsortedResult.sorted
  }

  private def uniqueIndex[K, V](iterable: Iterable[V])(keyMapper: V => K): Map[K, V] = {
    iterable.map(v => (keyMapper(v) -> v)).toMap
  }
}
