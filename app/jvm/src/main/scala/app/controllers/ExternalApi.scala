package app.controllers

import scala.collection.immutable.Seq
import hydro.models.access.DbQueryImplicits._
import app.models.access.JvmEntityAccess
import app.models.access.ModelFields
import app.models.access.ModelFields.TaskEntity
import app.models.document.DocumentEntity
import app.models.document.DocumentPermissionAndPlacement
import app.models.document.TaskEntity
import app.models.user.User
import app.models.user.Users
import com.google.inject.Inject
import hydro.common.CollectionUtils
import hydro.common.OrderToken
import hydro.common.time.Clock
import hydro.controllers.helpers.AuthenticatedAction
import hydro.models.modification.EntityModification
import play.api.i18n.I18nSupport
import play.api.i18n.MessagesApi
import play.api.mvc._

final class ExternalApi @Inject() (implicit
    override val messagesApi: MessagesApi,
    components: ControllerComponents,
    entityAccess: JvmEntityAccess,
    playConfiguration: play.api.Configuration,
    env: play.api.Environment,
    clock: Clock,
) extends AbstractController(components)
    with I18nSupport {

  def shareDocument(documentIdString: String, loginName: String, applicationSecret: String) = Action {
    implicit request =>
      validateApplicationSecret(applicationSecret)

      val document =
        entityAccess
          .newQuerySync[DocumentEntity]()
          .findById(documentIdString.toLong)
      val user =
        entityAccess
          .newQuerySync[User]()
          .findOne(ModelFields.User.loginName === loginName)
          .getOrElse(throw new IllegalArgumentException(s"Could not find user with loginName=$loginName"))

      requireNoExistingPermissions(user, document)
      val greatestOrderToken = getGreatestDocumentOrderToken(user)

      implicit val issuer = Users.getOrCreateRobotUser()
      entityAccess.persistEntityModifications(
        EntityModification.createAddWithRandomId(
          DocumentPermissionAndPlacement(
            documentId = document.id,
            userId = user.id,
            orderToken = OrderToken.middleBetween(greatestOrderToken, None),
          )
        )
      )

      Ok(s"OK\n")
  }

  def regenerateOrderTokens(dryOrWetRun: String, applicationSecret: String) = Action { implicit request =>
    validateApplicationSecret(applicationSecret)

    val allDocuments = entityAccess.newQuerySync[DocumentEntity]().data()

    val resultString = new StringBuilder()
    for (document <- allDocuments) {
      resultString.append(s"Processing docuemnt ${document.id}: ${document.name}\n")
      val allTasksInDocument = entityAccess
        .newQuerySync[TaskEntity]()
        .filter(ModelFields.TaskEntity.documentId === document.id)
        .data()
        .sortBy(_.orderToken)

      val orderTokens = allTasksInDocument.map(_.orderToken)

      resultString.append(s"  Found ${orderTokens.size} tasks\n")
      if (orderTokens.distinct != orderTokens) {
        resultString.append(
          s"  !! Found ${orderTokens.size - orderTokens.distinct.size} tasks with duplicate order tokens\n"
        )
      }

      val newOrderTokens = OrderToken.evenlyDistributedValuesBetween(
        allTasksInDocument.size,
        lowerExclusive = None,
        higherExclusive = None,
      )
      val taskUpdates =
        for {
          (task, newOrderToken) <- allTasksInDocument zip newOrderTokens
          if task.orderToken != newOrderToken
        } yield EntityModification.createUpdate(
          task.copy(orderToken = newOrderToken),
          Seq(ModelFields.TaskEntity.orderToken),
        )

      dryOrWetRun match {
        case "dry" =>
          resultString.append(s"  Would apply ${taskUpdates.size} updates in wet run\n")
        case "wet" =>
          entityAccess.persistEntityModifications(taskUpdates)(Users.getOrCreateRobotUser())
          resultString.append(s"  Applied ${taskUpdates.size} updates\n")
      }

      resultString.append("  Done\n")
      resultString.append("\n")
    }

    Ok(s"OK\n\n$resultString")
  }

  // ********** private helper methods ********** //
  private def getGreatestDocumentOrderToken(user: User): Option[OrderToken] = {
    val allPermissionAndPlacements =
      entityAccess
        .newQuerySync[DocumentPermissionAndPlacement]()
        .filter(ModelFields.DocumentPermissionAndPlacement.userId === user.id)
        .data()

    if (allPermissionAndPlacements.nonEmpty) Some(allPermissionAndPlacements.map(_.orderToken).max) else None
  }
  private def requireNoExistingPermissions(user: User, document: DocumentEntity): Unit = {
    val existingPermissions =
      entityAccess
        .newQuerySync[DocumentPermissionAndPlacement]()
        .filter(ModelFields.DocumentPermissionAndPlacement.documentId === document.id)
        .filter(ModelFields.DocumentPermissionAndPlacement.userId === user.id)
        .data()
    require(
      existingPermissions.isEmpty,
      s"This document has already been shared: existingPermissions = $existingPermissions",
    )

  }

  private def validateApplicationSecret(applicationSecret: String): Unit = {
    val realApplicationSecret: String = playConfiguration.get[String]("play.http.secret.key")
    require(
      applicationSecret == realApplicationSecret,
      s"Invalid application secret. Found '$applicationSecret' but should be '$realApplicationSecret'",
    )
  }
}
