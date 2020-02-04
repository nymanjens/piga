package app.scala2js

import java.time.Month.MARCH

import app.common.testing.TestObjects._
import app.models.document.DocumentEntity
import app.models.document.DocumentPermissionAndPlacement
import app.models.document.TaskEntity
import app.models.user.User
import app.scala2js.AppConverters._
import hydro.common.time.LocalDateTime
import hydro.scala2js.Scala2Js
import utest._

object AppConvertersTest extends TestSuite {
  val dateTime = LocalDateTime.of(2022, MARCH, 13, 12, 13)

  override def tests = TestSuite {

    "fromEntityType" - {
      fromEntityType(User.Type) ==> UserConverter
    }

    "UserConverter: testToJsAndBack" - {
      testToJsAndBack[User](testUserRedacted)
    }
    "DocumentEntityConverter: testToJsAndBack" - {
      testToJsAndBack[DocumentEntity](testDocumentEntity)
    }
    "DocumentPermissionAndPlacementConverter: testToJsAndBack" - {
      testToJsAndBack[DocumentPermissionAndPlacement](testDocumentPermissionAndPlacement)
    }
    "TaskEntityConverter: testToJsAndBack" - {
      testToJsAndBack[TaskEntity](testTaskEntity)
    }
  }

  private def testToJsAndBack[T: Scala2Js.Converter](value: T) = {
    val jsValue = Scala2Js.toJs[T](value)
    val generated = Scala2Js.toScala[T](jsValue)
    generated ==> value
  }
}
