package api

import api.Picklers._
import api.ScalaJsApi._
import boopickle.Default._
import boopickle.Pickler
import common.testing.TestObjects._
import common.testing._
import models.modification.{EntityModification, EntityType}
import models.user.User
import org.junit.runner._
import org.specs2.runner._

import scala.collection.immutable.Seq

@RunWith(classOf[JUnitRunner])
class PicklersTest extends HookedSpecification {

  "EntityType" in {
    testPickleAndUnpickle[EntityType.any](EntityType.UserType)
  }

  "EntityModification" in {
    testPickleAndUnpickle[EntityModification](EntityModification.Add(testUserRedacted))
    testPickleAndUnpickle[EntityModification](EntityModification.Update(testUserRedacted))
    testPickleAndUnpickle[EntityModification](EntityModification.Remove[User](123054))
  }

  "GetInitialDataResponse" in {
    testPickleAndUnpickle[GetInitialDataResponse](
      GetInitialDataResponse(
        user = testUserRedacted,
        i18nMessages = Map("abc" -> "def"),
        nextUpdateToken = testUpdateToken
      ))
  }

  "GetAllEntitiesResponse" in {
    testPickleAndUnpickle[GetAllEntitiesResponse](
      GetAllEntitiesResponse(
        entitiesMap = Map(EntityType.UserType -> Seq(testUserRedacted)),
        nextUpdateToken = testUpdateToken))
  }

  "ModificationsWithToken" in {
    testPickleAndUnpickle[ModificationsWithToken](
      ModificationsWithToken(modifications = Seq(testModification), nextUpdateToken = testUpdateToken))
  }

  private def testPickleAndUnpickle[T: Pickler](value: T) = {
    val bytes = Pickle.intoBytes[T](value)
    val unpickled = Unpickle[T].fromBytes(bytes)
    unpickled mustEqual value
  }
}
