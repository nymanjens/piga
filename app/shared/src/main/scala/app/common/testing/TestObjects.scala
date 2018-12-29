package app.common.testing
import java.time.Instant
import java.time.Month._

import app.api.ScalaJsApi.GetInitialDataResponse
import app.api.ScalaJsApi.UpdateToken
import app.api.ScalaJsApi.UserPrototype
import app.common.OrderToken
import hydro.common.time.LocalDateTime
import hydro.common.time.LocalDateTimes
import app.models.document.DocumentEntity
import app.models.document.TaskEntity
import app.models.modification.EntityModification
import app.models.user.User

import scala.collection.immutable.Seq

object TestObjects {

  val orderTokenA: OrderToken = OrderToken.middleBetween(None, Some(OrderToken.middle))
  val orderTokenB: OrderToken = OrderToken.middleBetween(Some(OrderToken.middle), None)
  val orderTokenC: OrderToken = OrderToken.middleBetween(Some(orderTokenB), None)
  val orderTokenD: OrderToken = OrderToken.middleBetween(Some(orderTokenC), None)
  val orderTokenE: OrderToken = OrderToken.middleBetween(Some(orderTokenD), None)

  val testDate: LocalDateTime = LocalDateTimes.createDateTime(2008, MARCH, 13)
  val testInstant = Instant.ofEpochMilli(999000111)
  val testUpdateToken: UpdateToken = s"123782:12378"

  def testUserA: User = User(
    loginName = "testUserA",
    passwordHash =
      "be196838736ddfd0007dd8b2e8f46f22d440d4c5959925cb49135abc9cdb01e84961aa43dd0ddb6ee59975eb649280d9f44088840af37451828a6412b9b574fc",
    // = sha512("pw")
    name = "Test User A",
    isAdmin = false,
    idOption = Option(918273)
  )
  val testUserB: User = User(
    loginName = "testUserB",
    passwordHash =
      "be196838736ddfd0007dd8b2e8f46f22d440d4c5959925cb49135abc9cdb01e84961aa43dd0ddb6ee59975eb649280d9f44088840af37451828a6412b9b574fc",
    // = sha512("pw")
    name = "Test User B",
    isAdmin = false,
    idOption = Option(918274)
  )
  def testUser: User = testUserA
  def testUserRedacted: User = testUser.copy(passwordHash = "<redacted>")

  val testUserPrototype = UserPrototype.create(
    id = testUser.id,
    loginName = testUser.loginName,
    plainTextPassword = "dlkfjasfd",
    name = testUser.name,
    isAdmin = testUser.isAdmin)

  val testDocumentEntity: DocumentEntity =
    DocumentEntity(name = "Some test document", orderToken = orderTokenA, idOption = Some(129830))
  val testTaskEntity: TaskEntity = TaskEntity(
    documentId = testDocumentEntity.id,
    contentHtml = "abc<b>def</b>",
    orderToken = orderTokenA,
    indentation = 12,
    collapsed = true,
    delayedUntil = Some(testDate),
    tags = Seq("tag"),
    idOption = Some(821379)
  )

  val testModificationA: EntityModification = EntityModification.Add(testTaskEntity)
  val testModificationB: EntityModification =
    EntityModification.Add(testUserB.copy(passwordHash = "<redacted>"))
  def testModification: EntityModification = testModificationA

  val testGetInitialDataResponse: GetInitialDataResponse = GetInitialDataResponse(
    user = testUserRedacted,
    allAccessibleDocuments = Seq(testDocumentEntity),
    i18nMessages = Map("abc" -> "def"),
    nextUpdateToken = testUpdateToken
  )
}
