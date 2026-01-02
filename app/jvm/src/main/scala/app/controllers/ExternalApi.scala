package app.controllers

import app.common.MarkdownConverter
import net.liftweb.json.DefaultFormats
import net.liftweb.json.Serialization

import scala.collection.immutable.Seq
import hydro.models.access.DbQueryImplicits._
import app.models.access.JvmEntityAccess
import app.models.access.ModelFields
import app.models.document.DocumentEntity
import app.models.document.DocumentPermissionAndPlacement
import app.models.document.TaskEntity
import app.models.user.User
import app.models.user.Users
import com.google.inject.Inject
import hydro.common.CollectionUtils
import hydro.common.OrderToken
import hydro.common.time.Clock
import hydro.common.ScalaUtils
import hydro.controllers.helpers.AuthenticatedAction
import hydro.models.modification.EntityModification
import play.api.i18n.I18nSupport
import play.api.i18n.MessagesApi
import play.api.mvc._

import java.net.URLDecoder
import java.nio.charset.StandardCharsets
import java.util.Base64

final class ExternalApi @Inject() (implicit
    override val messagesApi: MessagesApi,
    components: ControllerComponents,
    entityAccess: JvmEntityAccess,
    playConfiguration: play.api.Configuration,
    env: play.api.Environment,
    clock: Clock,
) extends AbstractController(components)
    with I18nSupport {

  // ********** External API ********** //

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

  def getTasksAsJson(
      documentIdString: String,
      parentTagEncoded: String,
      applicationSecret: String,
  ) =
    Action { implicit request =>
      validateApplicationSecret(applicationSecret)

      val documentId = documentIdString.toLong
      val parentTag = URLDecoder.decode(parentTagEncoded, "UTF-8")

      // Validate that document exists
      entityAccess
        .newQuerySync[DocumentEntity]()
        .findById(documentId)

      val tasks =
        entityAccess
          .newQuerySync[TaskEntity]()
          .filter(ModelFields.TaskEntity.documentId === documentId)
          .data()
          .sorted

      // Find parent task with given tag
      val parentAndBelow: Seq[TaskEntity] = tasks.dropWhile(t => !(t.tags contains parentTag))

      // Validate that the given tag exists in the document
      require(parentAndBelow.nonEmpty, s"Could not find $parentTag")
      val parent = parentAndBelow.head

      val tasksToReturn = parentAndBelow.takeWhile(t => t == parent || t.indentation > parent.indentation)

      Ok(toJson(tasksToReturn))
    }

  def insertTask(
      documentIdString: String,
      parentTagEncoded: String,
      contentEncoded: String,
      applicationSecret: String,
  ) =
    Action { implicit request =>
      validateApplicationSecret(applicationSecret)
      implicit val user = Users.getOrCreateRobotUser()

      insertTaskInternal(documentIdString, parentTagEncoded, contentEncoded)

      Ok(s"OK\n")
    }

  def regenerateOrderTokens(dryOrWetRun: String, applicationSecret: String) = Action { implicit request =>
    validateApplicationSecret(applicationSecret)

    val allDocuments = entityAccess.newQuerySync[DocumentEntity]().data()

    val resultString = new StringBuilder()
    for (document <- allDocuments) {
      resultString.append(s"Processing document ${document.id}: ${document.name}\n")
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

  // ********** Interactive API ********** //

  def interactiveDone() = Action { implicit request =>
    Ok(views.html.doneAndClose())
  }

  def interactiveInsertTask(documentIdString: String, parentTagEncoded: String, contentEncoded: String) =
    AuthenticatedAction(parse.raw) { implicit user => implicit request =>
      insertTaskInternal(documentIdString, parentTagEncoded, contentEncoded)
      Redirect(app.controllers.routes.ExternalApi.interactiveDone())
    }

  // ********** private helper methods ********** //
  private def insertTaskInternal(documentIdString: String, parentTagEncoded: String, contentEncoded: String)(
      implicit user: User
  ): Unit = {
    val documentId = documentIdString.toLong
    val parentTag = URLDecoder.decode(parentTagEncoded, "UTF-8")
    val parsedTasks = MarkdownConverter.markdownToParsedTasks(maybeDecodeAsBase64(contentEncoded))

    // Validate that document exists
    entityAccess
      .newQuerySync[DocumentEntity]()
      .findById(documentId)

    val tasks =
      entityAccess
        .newQuerySync[TaskEntity]()
        .filter(ModelFields.TaskEntity.documentId === documentId)
        .data()
        .sorted

    // Find parent task with given tag
    val parentAndBelow: Seq[TaskEntity] = tasks.dropWhile(t => !(t.tags contains parentTag))

    // Validate that the given tag exists in the document
    require(parentAndBelow.nonEmpty, s"Could not find $parentTag")
    val parent = parentAndBelow.head

    val orderTokens = {
      val lastChildIndex: Int = {
        var lastChildIndex = 0
        while (
          CollectionUtils.maybeGet(parentAndBelow, lastChildIndex + 1).isDefined &&
          parentAndBelow(lastChildIndex + 1).indentation > parent.indentation
        ) {
          lastChildIndex += 1
        }
        lastChildIndex
      }

      OrderToken.evenlyDistributedValuesBetween(
        numValues = parsedTasks.size,
        lowerExclusive = CollectionUtils.maybeGet(parentAndBelow, lastChildIndex).map(_.orderToken),
        higherExclusive = CollectionUtils.maybeGet(parentAndBelow, lastChildIndex + 1).map(_.orderToken),
      )
    }

    require(orderTokens.size == parsedTasks.size)

    // If this the first task is the only top level task, collapse it because it nicely encapsulates the whole added task
    val collapseFirstTask =
      parsedTasks.size > 1 && parsedTasks.head.relativeIndentation == 0 && parsedTasks.tail.forall(
        _.relativeIndentation > 0
      )

    entityAccess.persistEntityModifications(
      for (((parsedTask, orderToken), i) <- (parsedTasks zip orderTokens).zipWithIndex.toVector) yield {
        EntityModification.createAddWithRandomId(
          TaskEntity(
            documentId = documentId,
            contentHtml = parsedTask.html,
            orderToken = orderToken,
            indentation = parent.indentation + 1 + parsedTask.relativeIndentation,
            collapsed = i == 0 && collapseFirstTask,
            checked = false,
            delayedUntil = None,
            tags = parsedTask.tags,
            lastContentModifierUserId = user.id,
          )
        )
      }
    )
  }

  private def maybeDecodeAsBase64(str: String): String = {
    if (str.startsWith("base64:")) {
      new String(
        Base64.getDecoder.decode(ScalaUtils.stripRequiredPrefix(str, "base64:")),
        StandardCharsets.UTF_8,
      )
    } else {
      URLDecoder.decode(str, "UTF-8")
    }
  }

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

  private def toJson(tasks: Seq[TaskEntity]): String = {
    val normalizedTasks = {
      val root = tasks.head
      tasks.map(t => t.copy(indentation = t.indentation - root.indentation))
    }

    implicit val formats = DefaultFormats
    Serialization.write(normalizedTasks.map(JsonSerializableTask.fromTaskEntity))
  }

}
private case class JsonSerializableTask(
    contentHtml: String,
    indentation: Int,
    collapsed: Boolean,
    checked: Boolean,
    tags: Seq[String],
)
private object JsonSerializableTask {
  def fromTaskEntity(taskEntity: TaskEntity): JsonSerializableTask = {
    JsonSerializableTask(
      contentHtml = taskEntity.contentHtml,
      indentation = taskEntity.indentation,
      collapsed = taskEntity.collapsed,
      checked = taskEntity.checked,
      tags = taskEntity.tags,
    )
  }
}
