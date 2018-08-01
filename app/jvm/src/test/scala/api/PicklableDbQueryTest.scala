package api

import api.Picklers._
import common.testing._
import models.access.DbQueryImplicits._
import models.access.{DbQuery, ModelField}
import models.user.User
import org.junit.runner._
import org.specs2.runner._

import scala.collection.immutable.Seq

@RunWith(classOf[JUnitRunner])
class PicklableDbQueryTest extends HookedSpecification {

  "regular -> picklable -> regular" in {
    def testFromRegularToRegular(query: DbQuery[_]) = {
      PicklableDbQuery.fromRegular(query).toRegular mustEqual query
    }

    "null object" in {
      testFromRegularToRegular(
        DbQuery[User](filter = DbQuery.Filter.NullFilter(), sorting = None, limit = None))
    }
    "limit" in {
      testFromRegularToRegular(
        DbQuery[User](
          filter = DbQuery.Filter.NullFilter(),
          sorting = None,
          limit = Some(192)
        ))
    }
    "filters" in {
      val filters: Seq[DbQuery.Filter[User]] = Seq(
        (ModelField.User.loginName === "a") && (ModelField.User.loginName !== "b"),
        ModelField.User.name containsIgnoreCase "abc"
      )
      for (filter <- filters) yield {
        testFromRegularToRegular(DbQuery[User](filter = filter, sorting = None, limit = Some(192)))
      }
    }
  }
}
