package models.slick

import java.nio.ByteBuffer

import api.Picklers._
import boopickle.Default.{Pickle, Unpickle}
import common.accounting.Tags
import common.time.LocalDateTime
import models.Entity
import models.modification.{EntityModification, EntityModificationEntity}
import models.money.ExchangeRateMeasurement
import models.slick.SlickUtils.dbApi.{Table => SlickTable, Tag => SlickTag, _}
import models.slick.SlickUtils.localDateTimeToSqlDateMapper
import models.user.User

import scala.collection.immutable.Seq

sealed trait SlickEntityTableDef[E <: Entity] {
  type Table <: SlickEntityTableDef.EntityTable[E]
  def tableName: String
  def table(tag: SlickTag): Table
}

object SlickEntityTableDef {

  val all: Seq[SlickEntityTableDef[_]] =
    Seq(
      UserDef
    )

  /** Table extension to be used with an Entity model. */
  // Based on active-slick (https://github.com/strongtyped/active-slick)
  sealed abstract class EntityTable[E <: Entity](
      tag: SlickTag,
      tableName: String,
      schemaName: Option[String] = None)(implicit val colType: BaseColumnType[Long])
      extends SlickTable[E](tag, schemaName, tableName) {

    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  }

  implicit object UserDef extends SlickEntityTableDef[User] {

    override val tableName: String = "USERS"
    override def table(tag: SlickTag): Table = new Table(tag)

    /* override */
    final class Table(tag: SlickTag) extends EntityTable[User](tag, tableName) {
      def loginName = column[String]("loginName")
      def passwordHash = column[String]("passwordHash")
      def name = column[String]("name")
      def isAdmin = column[Boolean]("isAdmin")
      def expandCashFlowTablesByDefault = column[Boolean]("expandCashFlowTablesByDefault")
      def expandLiquidationTablesByDefault = column[Boolean]("expandLiquidationTablesByDefault")

      override def * =
        (
          loginName,
          passwordHash,
          name,
          isAdmin,
          expandCashFlowTablesByDefault,
          expandLiquidationTablesByDefault,
          id.?) <> (User.tupled, User.unapply)
    }
  }

  implicit object EntityModificationEntityDef extends SlickEntityTableDef[EntityModificationEntity] {

    override val tableName: String = "ENTITY_MODIFICATION_ENTITY"
    override def table(tag: SlickTag): Table = new Table(tag)

    /* override */
    final class Table(tag: SlickTag) extends EntityTable[EntityModificationEntity](tag, tableName) {
      def userId = column[Long]("userId")
      def change = column[EntityModification]("modification")
      def date = column[LocalDateTime]("date")

      override def * =
        (userId, change, date, id.?) <> (EntityModificationEntity.tupled, EntityModificationEntity.unapply)
    }

    implicit val entityModificationToBytesMapper: ColumnType[EntityModification] = {
      def toBytes(modification: EntityModification) = {
        val byteBuffer = Pickle.intoBytes(modification)

        val byteArray = new Array[Byte](byteBuffer.remaining)
        byteBuffer.get(byteArray)
        byteArray
      }
      def toEntityModification(bytes: Array[Byte]) = {
        val byteBuffer = ByteBuffer.wrap(bytes)
        Unpickle[EntityModification].fromBytes(byteBuffer)
      }
      MappedColumnType.base[EntityModification, Array[Byte]](toBytes, toEntityModification)
    }
  }
}
