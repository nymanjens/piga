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
import hydro.models.modification.EntityType
import org.reactivestreams.Subscriber
import org.reactivestreams.Subscription

final class AppEntityPermissions @Inject()(
    implicit
    entityAccess: JvmEntityAccess,
) extends EntityPermissions {

  // Initialize documentToOwners lazily, mainly to avoid test startup issues
  @volatile private var _documentToOwners: ImmutableMultimap[Long, Long] = _

  entityAccess.entityModificationPublisher.subscribe(EntityModificationSubscriber)

  // **************** Implementation of EntityPermissions API **************** //
  override def checkAllowedForWrite(modification: EntityModification)(implicit user: User): Unit = {
    EntityPermissions.DefaultImpl.checkAllowedForWrite(modification)

    modification.entityType match {
      case TaskEntity.Type =>
        modification.maybeEntity[TaskEntity] match {
          case Some(e) =>
            require(
              documentToOwners.containsEntry(e.documentId, user.id) ||
                // The very first task in a document is created simultaneously with the document itself, so if there
                // are no owners, allow an edit to happen
                documentToOwners.get(e.documentId).isEmpty,
              s"$user is trying to edit a document (id = ${e.documentId}) that they don't own"
            )
          case None => // Removal by ID is expensive to check and requires the ID, which is very hard to guess
        }

      case DocumentEntity.Type =>
        modification match {
          case EntityModification.Add(_) => // Adding a document is fine
          case EntityModification.Update(uncastEntity) =>
            val e = uncastEntity.asInstanceOf[DocumentEntity]
            require(
              documentToOwners.containsEntry(e.id, user.id),
              s"$user is trying to edit a document (id = ${e.id}) that they don't own")
          case EntityModification.Remove(_) =>
          // Removal by ID is expensive to check and requires the ID, which is very hard to guess
        }

      case DocumentPermissionAndPlacement.Type =>
        modification match {
          case EntityModification.Add(uncastEntity) => // Adding a permission is only allowed if the document has no owners
            val e = uncastEntity.asInstanceOf[DocumentPermissionAndPlacement]
            require(
              documentToOwners.get(e.documentId).isEmpty,
              s"$user is trying to add an additional owner to a document (id = ${e.documentId}) that is already " +
                s"owned by userId = ${documentToOwners.get(e.documentId)}"
            )
            require(
              e.userId == user.id,
              s"$user is trying to add an owner (userId = ${e.userId}) to a document (id = ${e.documentId}) that is " +
                s"not themselves")

          case EntityModification.Update(uncastEntity) =>
            val e = uncastEntity.asInstanceOf[DocumentPermissionAndPlacement]
            require(
              documentToOwners.containsEntry(e.documentId, user.id),
              s"$user is trying to edit a document (id = ${e.documentId}) that they don't own")
            require(
              e.userId == user.id,
              s"$user is trying to edit a permission for an owner (userId = ${e.userId}) to a document " +
                s"(id = ${e.documentId}) that is not themselves"
            )

          case EntityModification.Remove(_) =>
          // Removal by ID is expensive to check and requires the ID, which is very hard to guess
        }

      case _ => // Do nothing
    }
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

  // **************** Private helper methods **************** //
  private def documentToOwners: ImmutableMultimap[Long, Long] = {
    if (_documentToOwners == null) {
      recalculateDocumentToOwners()
    }
    _documentToOwners
  }

  private def recalculateDocumentToOwners(): Unit = {
    val resultBuilder = ImmutableMultimap.builder[Long, Long]()

    for (permission <- entityAccess.newQuerySync[DocumentPermissionAndPlacement]().data()) {
      resultBuilder.put(permission.documentId, permission.userId)
    }

    _documentToOwners = resultBuilder.build
  }

  private object EntityModificationSubscriber extends Subscriber[EntityModificationsWithToken] {
    override def onSubscribe(s: Subscription): Unit = {
      s.request(Long.MaxValue)
    }
    override def onNext(t: EntityModificationsWithToken): Unit = {
      for (modification <- t.modifications) {
        modification.entityType match {
          case DocumentPermissionAndPlacement.Type => recalculateDocumentToOwners()
          case _                                   => // Do nothing
        }
      }
    }
    override def onError(t: Throwable): Unit = {}
    override def onComplete(): Unit = {}
  }
}
