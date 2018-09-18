package tools

import java.nio.file.{Path, Paths}

import com.google.inject.Inject
import common.{OrderToken, ResourceFiles}
import common.time.Clock
import models.access.JvmEntityAccess
import models.{Entity, document}
import models.document.{DocumentEntity, TaskEntity}
import models.modification.EntityModification
import models.user.Users
import play.api.{Application, Mode}

import scala.collection.JavaConverters._

final class ApplicationStartHook @Inject()(implicit app: Application,
                                           entityAccess: JvmEntityAccess,
                                           clock: Clock) {
  onStart()

  private def onStart(): Unit = {
    processFlags()

    // Set up database if necessary
    if (app.mode == Mode.Test || app.mode == Mode.Dev) {
      if (AppConfigHelper.dropAndCreateNewDb) {
        dropAndCreateNewDb()
      }
    }

    // Populate the database with dummy data
    if (app.mode == Mode.Test || app.mode == Mode.Dev) {
      if (AppConfigHelper.loadDummyUsers) {
        loadDummyUsers()
      }
      if (AppConfigHelper.loadDummyData) {
        loadDummyData()
      }
    }
  }

  private def processFlags(): Unit = {
    if (CommandLineFlags.dropAndCreateNewDb) {
      println("")
      println("  Dropping the database tables (if present) and creating new ones...")
      dropAndCreateNewDb()
      println("  Done. Exiting.")

      System.exit(0)
    }

    if (CommandLineFlags.createAdminUser) {
      implicit val user = Users.getOrCreateRobotUser()

      val loginName = "admin"
      val password = AppConfigHelper.defaultPassword getOrElse "changeme"

      println("")
      println(s"  Creating admin user...")
      println(s"      loginName: $loginName")
      println(s"      password: $password")
      entityAccess.persistEntityModifications(
        EntityModification.createAddWithRandomId(
          Users.createUser(loginName, password, name = "Admin", isAdmin = true)))
      println("  Done. Exiting.")

      System.exit(0)
    }
  }

  private def dropAndCreateNewDb(): Unit = {
    entityAccess.dropAndCreateTables()
  }

  private def loadDummyUsers(): Unit = {
    implicit val user = Users.getOrCreateRobotUser()

    entityAccess.persistEntityModifications(
      EntityModification
        .createAddWithId(
          1111,
          Users.createUser(loginName = "admin", password = "a", name = "Admin", isAdmin = true)),
      EntityModification
        .createAddWithId(2222, Users.createUser(loginName = "alice", password = "a", name = "Alice")),
      EntityModification
        .createAddWithId(3333, Users.createUser(loginName = "bob", password = "b", name = "Bob"))
    )
  }

  private def loadDummyData(): Unit = {
    implicit val user = Users.getOrCreateRobotUser()

    val documentIdA = 12121212L
    val documentIdB = 34343434L
    entityAccess.persistEntityModifications(
      EntityModification.Add(
        DocumentEntity(
          name = "Test document B",
          orderToken = OrderToken.middleBetween(Some(OrderToken.middle), None),
          idOption = Some(documentIdB))),
      EntityModification.Add(
        DocumentEntity(
          name = "Test document A",
          orderToken = OrderToken.middle,
          idOption = Some(documentIdA))),
      EntityModification.Add(
        TaskEntity(
          documentId = documentIdA,
          contentHtml = "<b>Hello</b><br/>World",
          orderToken = OrderToken.middleBetween(None, Some(OrderToken.middle)),
          indentation = 0,
          idOption = Some(11)
        )),
      EntityModification.Add(
        TaskEntity(
          documentId = documentIdA,
          contentHtml = "<i>&lt;indented&gt;</i>",
          orderToken = OrderToken.middle,
          indentation = 2,
          idOption = Some(22))),
      EntityModification.Add(
        TaskEntity(
          documentId = documentIdA,
          contentHtml = """<a href="www.example.com">link to www.example.com</a>""",
          orderToken = OrderToken.middleBetween(Some(OrderToken.middle), None),
          indentation = 1,
          idOption = Some(33)
        )),
      EntityModification.Add(
        TaskEntity(
          documentId = documentIdB,
          contentHtml = "Second document",
          orderToken = OrderToken.middle,
          indentation = 0,
          idOption = Some(44)
        ))
    )
  }

  private def assertExists(path: Path): Path = {
    require(ResourceFiles.exists(path), s"Couldn't find path: $path")
    path
  }

  private object CommandLineFlags {
    private val properties = System.getProperties.asScala

    def dropAndCreateNewDb: Boolean = getBoolean("dropAndCreateNewDb")
    def createAdminUser: Boolean = getBoolean("createAdminUser")

    private def getBoolean(name: String): Boolean = properties.get(name).isDefined

    private def getExistingPath(name: String): Option[Path] =
      properties.get(name) map (Paths.get(_)) map assertExists
  }

  private object AppConfigHelper {
    def dropAndCreateNewDb: Boolean = getBoolean("app.development.dropAndCreateNewDb")
    def loadDummyUsers: Boolean = getBoolean("app.development.loadDummyUsers")
    def loadDummyData: Boolean = getBoolean("app.development.loadDummyData")
    def defaultPassword: Option[String] = getString("app.setup.defaultPassword")

    private def getBoolean(cfgPath: String): Boolean =
      app.configuration.getOptional[Boolean](cfgPath) getOrElse false

    private def getString(cfgPath: String): Option[String] =
      app.configuration.getOptional[String](cfgPath)

    private def getExistingPath(cfgPath: String): Path = assertExists {
      Paths.get(app.configuration.get[String](cfgPath))
    }
  }
}
