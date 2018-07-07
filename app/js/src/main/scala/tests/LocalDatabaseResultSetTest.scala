// TODO: Fix or remove

//package tests
//
//import models.access.DbQuery.Filter
//import models.access.DbQueryImplicits._
//import models.access._
//import models.access.webworker.LocalDatabaseWebWorkerApi
//import tests.ManualTests.{ManualTest, ManualTestSuite}
//
//import scala.async.Async.{async, await}
//import scala.collection.immutable.Seq
//import scala.concurrent.Future
//import scala.language.reflectiveCalls
//import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
//import scala2js.Converters._
//
//// Note that this is a manual test because the Rhino javascript engine used for tests
//// is incompatible with Loki.
//private[tests] class LocalDatabaseResultSetTest extends ManualTestSuite {
//
//  implicit private val webWorker: LocalDatabaseWebWorkerApi =
//    new models.access.webworker.Module().localDatabaseWebWorkerApiStub
//
//  override def tests = Seq(
//    // **************** Regular filter tests **************** //
//    ManualTest("queryExecutor().filter(nullFilter)") {
//      val user1 = createUser()
//      val user2 = createUser()
//      val user3 = createUser()
//
//      withUsers(user1, user2, user3)
//        .assertFilteredWith(Filter.NullFilter())
//        .containsExactly(user1, user2, user3)
//    },
//    ManualTest("queryExecutor().filter(equal)") {
//      val user1 = createUser()
//      val user2 = createUser()
//      val user3 = createUser()
//
//      withUsers(user1, user2, user3)
//        .assertFilteredWith(ModelField.id[User] === user2.id)
//        .containsExactly(user2)
//    },
//    ManualTest("queryExecutor().filter(notEqual)") {
//      val user1 = createUser()
//      val user2 = createUser()
//      val user3 = createUser()
//
//      withUsers(user1, user2, user3)
//        .assertFilteredWith(ModelField.id[User] !== user2.id)
//        .containsExactly(user1, user3)
//    },
//    ManualTest("queryExecutor().filter(lessThan)") {
//      val user1 = createUser(day = 1)
//      val user2 = createUser(day = 2)
//      val user3 = createUser(day = 3)
//
//      withUsers(user1, user2, user3)
//        .assertFilteredWith(ModelField.User.createdDate < user3.createdDate)
//        .containsExactly(user1, user2)
//    },
//    ManualTest("queryExecutor().filter(greaterThan)") {
//      val user1 = createUser(day = 1)
//      val user2 = createUser(day = 2)
//      val user3 = createUser(day = 3)
//
//      withUsers(user1, user2, user3)
//        .assertFilteredWith(ModelField.User.createdDate > user1.createdDate)
//        .containsExactly(user2, user3)
//    },
//    ManualTest("queryExecutor().filter(greaterOrEqualThan)") {
//      val user1 = createUser(day = 1)
//      val user2 = createUser(day = 2)
//      val user3 = createUser(day = 3)
//
//      withUsers(user1, user2, user3)
//        .assertFilteredWith(ModelField.User.createdDate >= user2.createdDate)
//        .containsExactly(user2, user3)
//    },
//    ManualTest("queryExecutor().filter(anyOf)") {
//      val user1 = createUser(category = testCategoryA)
//      val user2 = createUser(category = testCategoryB)
//      val user3 = createUser(category = testCategoryC)
//
//      withUsers(user1, user2, user3)
//        .assertFilteredWith(
//          ModelField.User.categoryCode isAnyOf Seq(testCategoryA.code, testCategoryB.code))
//        .containsExactly(user1, user2)
//    },
//    ManualTest("queryExecutor().filter(noneOf)") {
//      val user1 = createUser(category = testCategoryA)
//      val user2 = createUser(category = testCategoryB)
//      val user3 = createUser(category = testCategoryC)
//
//      withUsers(user1, user2, user3)
//        .assertFilteredWith(
//          ModelField.User.categoryCode isNoneOf Seq(testCategoryA.code, testCategoryB.code))
//        .containsExactly(user3)
//    },
//    ManualTest("queryExecutor().filter(containsIgnoreCase)") {
//      val user1 = createUser(description = "prefix\nAAAA_bbbb.*CCCC_dddd\nsuffix")
//      val user2 = createUser(description = "BBBB.*cccc")
//      val user3 = createUser(description = "prefix\nBBBBcccc\nsuffix")
//
//      withUsers(user1, user2, user3)
//        .assertFilteredWith(ModelField.User.description containsIgnoreCase "BBBB.*cccc")
//        .containsExactly(user1, user2)
//    },
//    ManualTest("queryExecutor().filter(doesntContainIgnoreCase)") {
//      val user1 = createUser(description = "prefix\nAAAA_bbbb.*CCCC_dddd\nsuffix")
//      val user2 = createUser(description = "BBBB.*cccc")
//      val user3 = createUser(description = "prefix\nBBBBcccc\nsuffix")
//
//      withUsers(user1, user2, user3)
//        .assertFilteredWith(ModelField.User.description doesntContainIgnoreCase "BBBB.*cccc")
//        .containsExactly(user3)
//    },
//    ManualTest("queryExecutor().filter(seqContains)") {
//      val user1 = createUser(tags = Seq("tagA", "tagB", "tag"))
//      val user2 = createUser(tags = Seq("tagA", "tagB"))
//      val user3 = createUser(tags = Seq("tag"))
//
//      withUsers(user1, user2, user3)
//        .assertFilteredWith(ModelField.User.tags contains "tag")
//        .containsExactly(user1, user3)
//    },
//    ManualTest("queryExecutor().filter(seqDoesntContain)") {
//      val user1 = createUser(tags = Seq("tagA", "tagB", "tag"))
//      val user2 = createUser(tags = Seq("tagA", "tagB"))
//      val user3 = createUser(tags = Seq("tag"))
//
//      withUsers(user1, user2, user3)
//        .assertFilteredWith(ModelField.User.tags doesntContain "tag")
//        .containsExactly(user2)
//    },
//    // **************** OR / AND filter tests **************** //
//    ManualTest("queryExecutor().filter(or(equal, anyOf))") {
//      val user1 = createUser()
//      val user2 = createUser()
//      val user3 = createUser()
//      val user4 = createUser()
//
//      withUsers(user1, user2, user3, user4)
//        .assertFilteredWith({
//          ModelField.id[User] === user1.id
//        } || {
//          ModelField.id[User] isAnyOf Seq(user2.id, user3.id)
//        })
//        .containsExactly(user1, user2, user3)
//    },
//    ManualTest("queryExecutor().filter(and(equal, equal))") {
//      val user1 = createUser(description = "abc", category = testCategoryA)
//      val user2 = createUser(description = "abc", category = testCategoryB)
//      val user3 = createUser(description = "def", category = testCategoryB)
//
//      withUsers(user1, user2, user3)
//        .assertFilteredWith({
//          ModelField.User.description === "abc"
//        } && {
//          ModelField.User.categoryCode === testCategoryB.code
//        })
//        .containsExactly(user2)
//    },
//    ManualTest("queryExecutor().filter(and(anyOf, anyOf))") {
//      val user1 = createUser()
//      val user2 = createUser()
//      val user3 = createUser()
//
//      withUsers(user1, user2, user3)
//        .assertFilteredWith({
//          ModelField.id[User] isAnyOf Seq(user2.id, user3.id)
//        } && {
//          ModelField.id[User] isAnyOf Seq(user1.id, user2.id)
//        })
//        .containsExactly(user2)
//    },
//    ManualTest("queryExecutor().filter(or(and(anyOf, anyOf), and(anyOf, anyOf))") {
//      val user1 = createUser()
//      val user2 = createUser()
//      val user3 = createUser()
//
//      withUsers(user1, user2, user3)
//        .assertFilteredWith(
//          {
//            (ModelField.id[User] isAnyOf Seq(user2.id, user3.id)) &&
//            (ModelField.id[User] isAnyOf Seq(user1.id, user2.id))
//          } || {
//            (ModelField.id[User] isAnyOf Seq(user1.id, user3.id)) &&
//            (ModelField.id[User] isAnyOf Seq(user2.id, user3.id))
//          }
//        )
//        .containsExactly(user2, user3)
//    },
//    // **************** Non-filter tests **************** //
//    ManualTest("queryExecutor().sort()") {
//      val user1 = createUser(groupId = 1, day = 2)
//      val user2 = createUser(groupId = 1, day = 3)
//      val user3 = createUser(groupId = 2, day = 1)
//
//      withUsers(user1, user2, user3)
//        .assertThat(
//          _.sort(DbQuery.Sorting
//            .descBy(ModelField.User.userGroupId)
//            .thenAscBy(ModelField.User.createdDate))
//            .data())
//        .containsExactlyInOrder(user3, user1, user2)
//    },
//    ManualTest("queryExecutor().limit()") {
//      val user1 = createUser(day = 1)
//      val user2 = createUser(day = 2)
//      val user3 = createUser(day = 3)
//
//      withUsers(user1, user2, user3)
//        .assertThat(
//          _.sort(DbQuery.Sorting.ascBy(ModelField.User.createdDate))
//            .limit(2)
//            .data())
//        .containsExactlyInOrder(user1, user2)
//    },
//    ManualTest("queryExecutor().findOne()") {
//      val user1 = createUser()
//      val user2 = createUser()
//      val user3 = createUser()
//
//      withUsers(user1, user2, user3)
//        .assertThat(_.findOne(ModelField.id, user2.id))
//        .isEqualTo(Some(user2))
//    },
//    ManualTest("queryExecutor().count()") {
//      val user1 = createUser()
//      val user2 = createUser()
//      val user3 = createUser()
//
//      withUsers(user1, user2, user3).assertThat(_.count()).isEqualTo(3)
//    }
//  )
//
//  private def withUsers(users: User*) = new Object {
//    def assertFilteredWith(filter: Filter[User]) = assertThat(_.filter(filter).data())
//
//    def assertThat(resultSetFunc: DbResultSet.Async[User] => Future[Any]) = new Object {
//      def containsExactly(expected: User*): Future[Unit] = async {
//        val db = await(LocalDatabaseImpl.createInMemoryForTests())
//        await(db.resetAndInitialize())
//        await(db.addAll(users.toVector))
//        await(resultSetFunc(DbResultSet.fromExecutor(db.queryExecutor[User]()))) match {
//          case seq: Seq[_] => assertEqualIterables(seq.toSet, expected.toSet)
//        }
//      }
//
//      def containsExactlyInOrder(expected: User*): Future[Unit] = async {
//        val db = await(LocalDatabaseImpl.createInMemoryForTests())
//        await(db.resetAndInitialize())
//        await(db.addAll(users.toVector))
//        await(resultSetFunc(DbResultSet.fromExecutor(db.queryExecutor[User]()))) match {
//          case seq: Seq[_] => assertEqualIterables(seq, expected.toVector)
//        }
//      }
//
//      def isEqualTo(expected: Any): Future[Unit] = async {
//        val db = await(LocalDatabaseImpl.createInMemoryForTests())
//        await(db.resetAndInitialize())
//        await(db.addAll(users.toVector))
//        await(resultSetFunc(DbResultSet.fromExecutor(db.queryExecutor[User]()))) ==> expected
//      }
//    }
//
//    private def assertEqualIterables(iterable1: Iterable[_], iterable2: Iterable[User]): Unit = {
//      def assertProperty(propertyFunc: User => Any): Unit = {
//        iterable1.map(_.asInstanceOf[User]).map(propertyFunc) ==> iterable2.map(propertyFunc)
//      }
//      assertProperty(_.description)
//      assertProperty(_.detailDescription)
//      assertProperty(_.categoryCode)
//      assertProperty(_.tags.mkString(","))
//      assertProperty(_.createdDate)
//      assertProperty(_.userGroupId)
//      assertProperty(_.id)
//      iterable1 ==> iterable2
//    }
//  }
//}
