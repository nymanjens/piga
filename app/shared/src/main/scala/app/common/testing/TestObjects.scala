package app.common.testing
import java.time.Instant
import java.time.Month._

import app.api.ScalaJsApi.GetInitialDataResponse
import app.api.ScalaJsApi.UpdateToken
import app.api.ScalaJsApi.UserPrototype
import app.common.document.UserDocument
import hydro.common.OrderToken
import app.models.document.DocumentEntity
import app.models.document.DocumentPermissionAndPlacement
import app.models.document.TaskEntity
import hydro.models.modification.EntityModification
import app.models.user.User
import hydro.common.time.LocalDateTime
import hydro.common.time.LocalDateTimes
import hydro.models.UpdatableEntity.LastUpdateTime

import scala.collection.immutable.Seq

object TestObjects {

  def orderTokenA: OrderToken = OrderToken.middleBetween(None, Some(OrderToken.middle))
  def orderTokenB: OrderToken = OrderToken.middleBetween(Some(OrderToken.middle), None)
  def orderTokenC: OrderToken = OrderToken.middleBetween(Some(orderTokenB), None)
  def orderTokenD: OrderToken = OrderToken.middleBetween(Some(orderTokenC), None)
  def orderTokenE: OrderToken = OrderToken.middleBetween(Some(orderTokenD), None)
  def testOrderToken: OrderToken = orderTokenC

  def testDate: LocalDateTime = LocalDateTimes.createDateTime(2008, MARCH, 13)
  def testInstantA: Instant = Instant.ofEpochMilli(999000001)
  def testInstantB: Instant = Instant.ofEpochMilli(999000002)
  def testInstantC: Instant = Instant.ofEpochMilli(999000003)
  def testInstantD: Instant = Instant.ofEpochMilli(999000004)
  def testInstant: Instant = testInstantA
  def testUpdateToken: UpdateToken = s"123782:12378"

  def testLastUpdateTime = LastUpdateTime.allFieldsUpdated(testInstant)

  def testUserA: User = User(
    loginName = "testUserA",
    passwordHash =
      "be196838736ddfd0007dd8b2e8f46f22d440d4c5959925cb49135abc9cdb01e84961aa43dd0ddb6ee59975eb649280d9f44088840af37451828a6412b9b574fc",
    // = sha512("pw")
    name = "Test User A",
    isAdmin = false,
    idOption = Option(918273),
    lastUpdateTime = testLastUpdateTime,
  )
  def testUserB: User = User(
    loginName = "testUserB",
    passwordHash =
      "be196838736ddfd0007dd8b2e8f46f22d440d4c5959925cb49135abc9cdb01e84961aa43dd0ddb6ee59975eb649280d9f44088840af37451828a6412b9b574fc",
    // = sha512("pw")
    name = "Test User B",
    isAdmin = false,
    idOption = Option(918274),
    lastUpdateTime = testLastUpdateTime,
  )
  def testUser: User = testUserA
  def testUserRedacted: User = testUser.copy(passwordHash = "<redacted>")

  def testUserPrototype =
    UserPrototype.create(
      id = testUser.id,
      loginName = testUser.loginName,
      plainTextPassword = "dlkfjasfd",
      name = testUser.name,
      isAdmin = testUser.isAdmin,
    )

  def testDocumentEntity: DocumentEntity =
    DocumentEntity(name = "Some test document", idOption = Some(129830), lastUpdateTime = testLastUpdateTime)
  def testDocumentPermissionAndPlacement: DocumentPermissionAndPlacement =
    DocumentPermissionAndPlacement(
      documentId = testDocumentEntity.id,
      userId = testUser.id,
      orderToken = orderTokenA,
      idOption = Some(129830),
      lastUpdateTime = testLastUpdateTime,
    )
  def testUserDocument: UserDocument = UserDocument(
    documentId = testDocumentEntity.id,
    name = testDocumentEntity.name,
    orderToken = orderTokenA,
  )
  def testTaskEntity: TaskEntity = TaskEntity(
    documentId = testDocumentEntity.id,
    contentHtml = "abc<b>def</b>",
    orderToken = orderTokenA,
    indentation = 12,
    collapsed = true,
    checked = true,
    delayedUntil = Some(testDate),
    tags = Seq("tag"),
    idOption = Some(821379),
    lastUpdateTime = testLastUpdateTime,
    lastContentModifierUserId = testUser.id,
  )

  def testModificationA: EntityModification = EntityModification.Add(testTaskEntity)
  def testModificationB: EntityModification =
    EntityModification.Add(testUserB.copy(passwordHash = "<redacted>"))
  def testModification: EntityModification = testModificationA

  def testGetInitialDataResponse: GetInitialDataResponse = GetInitialDataResponse(
    user = testUserRedacted,
    allAccessibleDocuments = Seq(testUserDocument),
    i18nMessages = Map("abc" -> "def"),
    nextUpdateToken = testUpdateToken,
  )
}
