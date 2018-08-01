package models.access

import com.google.inject._
import common.GuavaReplacement.Iterables.getOnlyElement
import common.testing.TestObjects._
import common.testing._
import models.modification.{EntityModification, EntityModificationEntity}
import models.slick.SlickUtils.dbRun
import models.user.User
import org.junit.runner._
import org.specs2.runner._
import play.api.test._

import scala.collection.immutable.Seq

@RunWith(classOf[JUnitRunner])
class JvmEntityAccessTest extends HookedSpecification {

  implicit private val adminUser = createUser().copy(loginName = "admin")

  @Inject implicit private val fakeClock: FakeClock = null

  @Inject private val entityAccess: JvmEntityAccess = null

  override def before() = {
    Guice.createInjector(new TestModule).injectMembers(this)
  }

  "persistEntityModifications()" in {
    "Persists EntityModification" in new WithApplication {
      fakeClock.setTime(testDate)

      entityAccess.persistEntityModifications(testModification)

      val modificationEntity = getOnlyElement(dbRun(entityAccess.newSlickQuery[EntityModificationEntity]()))
      modificationEntity.userId mustEqual adminUser.id
      modificationEntity.modification mustEqual testModification
      modificationEntity.date mustEqual testDate
    }

    "EntityModification.Add" in new WithApplication {
      val user = createUser()

      entityAccess.persistEntityModifications(EntityModification.Add(user))

      entityAccess.newQuerySync[User]().data() mustEqual Seq(user)
    }

    "EntityModification.Update" in new WithApplication {
      val user1 = createUser()
      val updatedUser1 = user1.copy(name = "other nme")
      entityAccess.persistEntityModifications(EntityModification.Add(user1))

      entityAccess.persistEntityModifications(EntityModification.createUpdate(updatedUser1))

      entityAccess.newQuerySync[User]().data() mustEqual Seq(updatedUser1)
    }

    "EntityModification.Delete" in new WithApplication {
      val user1 = createUser()
      entityAccess.persistEntityModifications(EntityModification.Add(user1))

      entityAccess.persistEntityModifications(EntityModification.createDelete(user1))

      entityAccess.newQuerySync[User]().data() mustEqual Seq()
    }

    "EntityModification.Add is idempotent" in new WithApplication {
      val user1 = createUser()
      val updatedUser1 = user1.copy(name = "other name")
      val user2 = createUser()

      entityAccess.persistEntityModifications(
        EntityModification.Add(user1),
        EntityModification.Add(user1),
        EntityModification.Add(updatedUser1),
        EntityModification.Add(user2)
      )

      entityAccess.newQuerySync[User]().data().toSet mustEqual Set(user1, user2)
    }

    "EntityModification.Update is idempotent" in new WithApplication {
      val user1 = createUser()
      val updatedUser1 = user1.copy(name = "other name")
      val user2 = createUser()
      entityAccess.persistEntityModifications(EntityModification.Add(user1))

      entityAccess.persistEntityModifications(
        EntityModification.Update(updatedUser1),
        EntityModification.Update(updatedUser1),
        EntityModification.Update(user2)
      )

      entityAccess.newQuerySync[User]().data() mustEqual Seq(updatedUser1)
    }

    "EntityModification.Delete is idempotent" in new WithApplication {
      val user1 = createUser()
      val user2 = createUser()
      val user3 = createUser()
      entityAccess.persistEntityModifications(EntityModification.Add(user1))
      entityAccess.persistEntityModifications(EntityModification.Add(user2))

      entityAccess.persistEntityModifications(
        EntityModification.createDelete(user2),
        EntityModification.createDelete(user2),
        EntityModification.createDelete(user3)
      )

      entityAccess.newQuerySync[User]().data() mustEqual Seq(user1)
    }
  }

  private def createUser(): User = testUser.copy(idOption = Some(EntityModification.generateRandomId()))
}
