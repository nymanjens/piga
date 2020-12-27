package app.controllers

import app.common.document.UserDocument
import app.models.access.JvmEntityAccess
import com.google.inject.Inject
import hydro.common.time.Clock
import hydro.controllers.helpers.AuthenticatedAction
import net.liftweb.json.DefaultFormats
import net.liftweb.json.Serialization
import play.api.i18n.I18nSupport
import play.api.i18n.MessagesApi
import play.api.mvc._

import scala.collection.immutable.Seq
import scala.concurrent.duration._
import scala.concurrent.Await

final class Webmanifest @Inject() (implicit
    override val messagesApi: MessagesApi,
    components: ControllerComponents,
    entityAccess: JvmEntityAccess,
    playConfiguration: play.api.Configuration,
    env: play.api.Environment,
    clock: Clock,
) extends AbstractController(components)
    with I18nSupport {

  def webmanifest = Action { request =>
    Ok(
      toJsonString(
        Webmanifest.Wrapper(
          name = "Task Keeper",
          short_name = "Task Keeper",
          start_url = "/app/",
          scope = "/",
          display = "standalone",
          background_color = "#fff",
          theme_color = "#f8f8f8",
          description = "Task Keeper",
          icons = Seq(
            Webmanifest.Icon(
              src = "/assets/images/favicon48x48.png",
              sizes = "48x48",
              `type` = "image/png",
            ),
            Webmanifest.Icon(
              src = "/assets/images/favicon192x192.png",
              sizes = "72x72 96x96 144x144 168x168 192x192",
              `type` = "image/png",
            ),
            Webmanifest.Icon(
              src = "/assets/images/favicon512x512.png",
              sizes = "512x512",
              `type` = "image/png",
            ),
          ),
          shortcuts =
            for (document <- getAccessibleDocuments(request))
              yield Webmanifest.Shortcut(
                name = document.name,
                url = s"/app/tasklist/${document.documentId}",
                icons = Seq(
                  // Disabled because I use this as shortcut to particular document
                  // Webmanifest.Icon(
                  //   src = "/assets/images/document_shortcut_96x96.png",
                  //   sizes = "96x96",
                  //   `type` = "image/png",
                  // ),
                  Webmanifest.Icon(
                    src = "/assets/images/favicon48x48.png",
                    sizes = "48x48",
                    `type` = "image/png",
                  ),
                  Webmanifest.Icon(
                    src = "/assets/images/favicon192x192.png",
                    sizes = "72x72 96x96 144x144 168x168 192x192",
                    `type` = "image/png",
                  ),
                  Webmanifest.Icon(
                    src = "/assets/images/favicon192x192.png",
                    sizes = "96x96",
                    `type` = "image/png",
                  ),
                  Webmanifest.Icon(
                    src = "/assets/images/favicon512x512.png",
                    sizes = "512x512",
                    `type` = "image/png",
                  ),
                ),
              ),
        )
      )
    )
  }

  private def getAccessibleDocuments(request: Request[AnyContent]): Seq[UserDocument] = {
    AuthenticatedAction.getAuthenticatedUser(request) match {
      case None =>
        Seq()
      case Some(user) =>
        implicit val _ = user
        Await.result(UserDocument.fetchAllForUser(), 5.seconds)
    }
  }

  private def toJsonString(manifest: Webmanifest.Wrapper): String = {
    implicit val formats = DefaultFormats
    Serialization.write(manifest)
  }
}
private object Webmanifest {
  case class Wrapper(
      name: String,
      short_name: String,
      start_url: String,
      scope: String,
      display: String,
      background_color: String,
      theme_color: String,
      description: String,
      icons: Seq[Icon],
      shortcuts: Seq[Shortcut],
  )
  case class Icon(src: String, sizes: String, `type`: String)
  case class Shortcut(name: String, url: String, icons: Seq[Icon])
}
