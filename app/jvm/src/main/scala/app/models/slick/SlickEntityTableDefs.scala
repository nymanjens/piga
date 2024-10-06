package app.models.slick

import app.models.document.DocumentEntity
import app.models.document.DocumentPermissionAndPlacement
import app.models.document.TaskEntity
import app.models.user.User
import hydro.common.OrderToken
import hydro.common.time.LocalDateTime
import hydro.common.GuavaReplacement.Splitter
import hydro.models.slick.SlickEntityTableDef
import hydro.models.slick.SlickEntityTableDef.EntityTable
import hydro.models.slick.SlickUtils._
import hydro.models.slick.SlickUtils.dbApi._
import hydro.models.slick.SlickUtils.dbApi.{Tag => SlickTag}
import hydro.models.slick.SlickUtils.lastUpdateTimeToBytesMapper
import hydro.models.slick.SlickUtils.orderTokenToBytesMapper
import hydro.models.UpdatableEntity.LastUpdateTime
import hydro.models.slick.StandardSlickEntityTableDefs.EntityModificationEntityDef

import scala.collection.immutable.Seq

object SlickEntityTableDefs {

  val all: Seq[SlickEntityTableDef[_]] = Seq(
    UserDef,
    DocumentEntityDef,
    DocumentPermissionAndPlacementDef,
    TaskEntityDef,
    EntityModificationEntityDef,
  )

  implicit object UserDef extends SlickEntityTableDef[User] {

    override val tableName: String = "USERS"
    override def table(tag: SlickTag): Table = new Table(tag)

    /* override */
    final class Table(tag: SlickTag) extends EntityTable[User](tag, tableName) {
      def loginName = column[String]("loginName")
      def passwordHash = column[String]("passwordHash")
      def name = column[String]("name")
      def isAdmin = column[Boolean]("isAdmin")
      def lastUpdateTime = column[LastUpdateTime]("lastUpdateTime")

      override def * =
        (loginName, passwordHash, name, isAdmin, id.?, lastUpdateTime) <> (User.tupled, User.unapply)
    }
  }

  implicit object DocumentEntityDef extends SlickEntityTableDef[DocumentEntity] {

    override val tableName: String = "DOCUMENT_ENTITIES"
    override def table(tag: SlickTag): Table = new Table(tag)

    /* override */
    final class Table(tag: SlickTag) extends EntityTable[DocumentEntity](tag, tableName) {
      def name = column[String]("name")
      def lastUpdateTime = column[LastUpdateTime]("lastUpdateTime")

      override def * =
        (name, id.?, lastUpdateTime) <> (DocumentEntity.tupled, DocumentEntity.unapply)
    }
  }
  implicit object DocumentPermissionAndPlacementDef
      extends SlickEntityTableDef[DocumentPermissionAndPlacement] {

    override val tableName: String = "DOCUMENT_PERMISSION_AND_PLACEMENTS"
    override def table(tag: SlickTag): Table = new Table(tag)

    /* override */
    final class Table(tag: SlickTag) extends EntityTable[DocumentPermissionAndPlacement](tag, tableName) {
      def documentId = column[Long]("documentId")
      def userId = column[Long]("userId")
      def orderToken = column[OrderToken]("orderToken")
      def lastUpdateTime = column[LastUpdateTime]("lastUpdateTime")

      override def * =
        (
          documentId,
          userId,
          orderToken,
          id.?,
          lastUpdateTime,
        ) <> (DocumentPermissionAndPlacement.tupled, DocumentPermissionAndPlacement.unapply)
    }
  }

  implicit object TaskEntityDef extends SlickEntityTableDef[TaskEntity] {

    override val tableName: String = "TASK_ENTITIES"
    override def table(tag: SlickTag): Table = new Table(tag)

    private implicit val tagsSeqToStringMapper: ColumnType[Seq[String]] = {
      MappedColumnType.base[Seq[String], String](_.mkString(","), Splitter.on(',').omitEmptyStrings().split)
    }

    /* override */
    final class Table(tag: SlickTag) extends EntityTable[TaskEntity](tag, tableName) {
      def documentId = column[Long]("documentId")
      def contentHtml = column[String]("contentHtml")
      def orderToken = column[OrderToken]("orderToken")
      def indentation = column[Int]("indentation")
      def collapsed = column[Boolean]("collapsed")
      def checked = column[Boolean]("checked")
      def delayedUntil = column[Option[LocalDateTime]]("delayedUntil")
      def tags = column[Seq[String]]("tagsString")(tagsSeqToStringMapper)
      def lastContentModifierUserId = column[Long]("lastContentModifierUserId")
      def lastUpdateTime = column[LastUpdateTime]("lastUpdateTime")

      override def * =
        (
          documentId,
          contentHtml,
          orderToken,
          indentation,
          collapsed,
          checked,
          delayedUntil,
          tags,
          lastContentModifierUserId,
          id.?,
          lastUpdateTime,
        ) <> (TaskEntity.tupled, TaskEntity.unapply)
    }
  }
}
