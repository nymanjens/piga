package app.api

import app.api.Picklers._
import app.api.ScalaJsApi._
import boopickle.Default._
import boopickle.Pickler
import common.testing.TestObjects._
import common.testing._
import app.models.modification.EntityModification
import app.models.modification.EntityType
import app.models.user.User
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
    testPickleAndUnpickle[GetInitialDataResponse](testGetInitialDataResponse)
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
