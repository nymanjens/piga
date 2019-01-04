package app.models.access

import app.models.document.DocumentEntity
import app.models.document.TaskEntity
import app.models.slick.SlickEntityTableDefs
import app.models.user.User
import com.google.inject._
import hydro.common.time.Clock
import hydro.models.modification.EntityType
import hydro.models.slick.SlickEntityTableDef

final class JvmEntityAccess @Inject()(implicit clock: Clock) extends JvmEntityAccessBase {

  protected def getEntityTableDef(entityType: EntityType.any): SlickEntityTableDef[entityType.get] = {
    val tableDef = entityType match {
      case User.Type           => SlickEntityTableDefs.UserDef
      case DocumentEntity.Type => SlickEntityTableDefs.DocumentEntityDef
      case TaskEntity.Type     => SlickEntityTableDefs.TaskEntityDef
    }
    tableDef.asInstanceOf[SlickEntityTableDef[entityType.get]]
  }
}
