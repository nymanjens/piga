package common.testing

import java.time.Month
import java.time.Month._

import api.ScalaJsApi.{GetInitialDataResponse, UpdateToken}
import common.time.LocalDateTimes.createDateTime
import common.time.{LocalDateTime, LocalDateTimes}
import models.accounting.config.Account.SummaryTotalRowDef
import models.accounting.config._
import models.accounting.{BalanceCheck, Transaction, TransactionGroup}
import models.modification.EntityModification
import models.money.ExchangeRateMeasurement
import models.user.User

import scala.collection.immutable.{ListMap, Seq}
import scala.util.Random

object TestObjects {

  def testUserA: User = User(
    loginName = "testUserA",
    passwordHash =
      "be196838736ddfd0007dd8b2e8f46f22d440d4c5959925cb49135abc9cdb01e84961aa43dd0ddb6ee59975eb649280d9f44088840af37451828a6412b9b574fc",
    // = sha512("pw")
    name = "Test User A",
    isAdmin = false,
    expandCashFlowTablesByDefault = true,
    expandLiquidationTablesByDefault = true,
    idOption = Option(918273)
  )
  val testUserB: User = User(
    loginName = "testUserB",
    passwordHash =
      "be196838736ddfd0007dd8b2e8f46f22d440d4c5959925cb49135abc9cdb01e84961aa43dd0ddb6ee59975eb649280d9f44088840af37451828a6412b9b574fc",
    // = sha512("pw")
    name = "Test User B",
    isAdmin = false,
    expandCashFlowTablesByDefault = true,
    expandLiquidationTablesByDefault = true,
    idOption = Option(918274)
  )
  def testUser: User = testUserA
  def testUserRedacted: User = testUser.copy(passwordHash = "<redacted>")

  val testDate: LocalDateTime = LocalDateTimes.createDateTime(2008, MARCH, 13)
  val testUpdateToken: UpdateToken = s"123782:12378"

  val testModificationA: EntityModification = EntityModification.Add(testUserA)
  val testModificationB: EntityModification = EntityModification.Add(testUserB)
  def testModification: EntityModification = testModificationA

  val testGetInitialDataResponse: GetInitialDataResponse = GetInitialDataResponse(
    user = testUserA,
    i18nMessages = Map(),
    nextUpdateToken = "1234:5678"
  )
}
