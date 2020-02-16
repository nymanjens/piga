package app.api

import app.api.ScalaJsApi.HydroPushSocketPacket.EntityModificationsWithToken
import app.models.access.JvmEntityAccess
import app.models.document.DocumentEntity
import app.models.document.DocumentPermissionAndPlacement
import app.models.document.TaskEntity
import app.models.user.User
import com.google.common.collect.ImmutableMultimap
import com.google.inject.Inject
import hydro.api.EntityPermissions
import hydro.models.modification.EntityModification
import hydro.models.Entity
import org.reactivestreams.Subscriber
import org.reactivestreams.Subscription

final class AppEntityPermissions @Inject()(
    implicit
    entityAccess: JvmEntityAccess,
) extends EntityPermissions {

  @volatile private var documentToOwners: ImmutableMultimap[Long, Long] = recalculateDocumentToOwners()

  entityAccess.entityModificationPublisher.subscribe(EntityModificationSubscriber)

  override def checkAllowedForWrite(modification: EntityModification)(implicit user: User): Unit = {
    EntityPermissions.DefaultImpl.checkAllowedForWrite(modification)
  }

  override def isAllowedToRead(entity: Entity)(implicit user: User): Boolean = {
    val isDocumentAccesssible = entity match {
      case e: TaskEntity => // Hide document content from unauthorized readers
        documentToOwners.containsEntry(e.documentId, user.id)
      case e: DocumentEntity => // Don't expose document names to unauthorized readers
        documentToOwners.containsEntry(e.id, user.id)
      case _: DocumentPermissionAndPlacement => true // Allow to see sharing relationships of all users
      case _                                 => true
    }

    EntityPermissions.DefaultImpl.isAllowedToRead(entity) && isDocumentAccesssible
  }

  private def recalculateDocumentToOwners(): ImmutableMultimap[Long, Long] = {
    val resultBuilder = ImmutableMultimap.builder[Long, Long]()

    for (permission <- entityAccess.newQuerySync[DocumentPermissionAndPlacement]().data()) {
      resultBuilder.put(permission.documentId, permission.userId)
    }

    resultBuilder.build
  }

  private object EntityModificationSubscriber extends Subscriber[EntityModificationsWithToken] {
    override def onSubscribe(s: Subscription): Unit = {
      s.request(Long.MaxValue)
    }
    override def onNext(t: EntityModificationsWithToken): Unit = {
      for (modification <- t.modifications) {
        modification.entityType match {
          case DocumentPermissionAndPlacement.Type =>
            documentToOwners = recalculateDocumentToOwners()
          case _ => // Do nothing
        }
      }
    }
    override def onError(t: Throwable): Unit = {}
    override def onComplete(): Unit = {}
  }
}
