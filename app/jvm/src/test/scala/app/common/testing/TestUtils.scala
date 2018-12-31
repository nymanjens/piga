package app.common.testing

import java.time.Instant
import java.time.ZoneId

import hydro.common.time.LocalDateTime
import hydro.models.Entity
import app.models.access.JvmEntityAccess
import app.models.modification.EntityModification
import app.models.modification.EntityType
import app.models.document.TaskEntity
import app.models.document.DocumentEntity
import app.models.user.User
import app.models.user.User

object TestUtils {

  def persist[E <: Entity: EntityType](entity: E)(implicit entityAccess: JvmEntityAccess): E = {
    implicit val user = User(
      idOption = Some(9213982174887321L),
      loginName = "robot",
      passwordHash = "Some hash",
      name = "Robot",
      isAdmin = false
    )
    val addition =
      if (entity.idOption.isDefined) EntityModification.Add(entity)
      else EntityModification.createAddWithRandomId(entity)
    entityAccess.persistEntityModifications(addition)
    addition.entity
  }

  def localDateTimeOfEpochSecond(milli: Long): LocalDateTime = {
    val instant = Instant.ofEpochSecond(milli).atZone(ZoneId.of("Europe/Paris"))
    LocalDateTime.of(
      instant.toLocalDate,
      instant.toLocalTime
    )
  }
}
