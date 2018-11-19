package flux.react.router

import common.I18n
import common.LoggingUtils.{LogExceptionsCallback, logExceptions}
import flux.action.{Action, Dispatcher}
import flux.stores.document.AllDocumentsStore
import japgolly.scalajs.react.extra.router.StaticDsl.RouteB
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._

import scala.reflect.ClassTag

private[router] final class RouterFactory(implicit reactAppModule: flux.react.app.Module,
                                          dispatcher: Dispatcher,
                                          i18n: I18n,
                                          allDocumentsStore: AllDocumentsStore) {

  def createRouter(): Router[Page] = {
    Router(BaseUrl.until(RouterFactory.pathPrefix), routerConfig)
  }

  private def routerConfig(implicit reactAppModule: flux.react.app.Module) = {
    RouterConfigDsl[Page]
      .buildConfig { dsl =>
        import dsl._
        val codeString: RouteB[String] = string("[a-zA-Z0-9_-]+")
        val returnToPath: RouteB[Option[String]] = ("?returnto=" ~ string(".+")).option
        val query: RouteB[String] = "?q=" ~ string(".+")

        def staticRuleFromPage(page: Page, renderer: RouterContext => VdomElement): dsl.Rule = {
          val path = RouterFactory.pathPrefix + page.getClass.getSimpleName.toLowerCase
          staticRoute(path, page) ~> renderR(ctl => logExceptions(renderer(RouterContext(page, ctl))))
        }
        def dynamicRuleFromPage[P <: Page](dynamicPart: String => RouteB[P])(
            renderer: (P, RouterContext) => VdomElement)(implicit pageClass: ClassTag[P]): dsl.Rule = {
          val staticPathPart = RouterFactory.pathPrefix + pageClass.runtimeClass.getSimpleName.toLowerCase
          val path = dynamicPart(staticPathPart)
          dynamicRouteCT(path) ~> dynRenderR {
            case (page, ctl) => logExceptions(renderer(page, RouterContext(page, ctl)))
          }
        }

        // wrap/connect components to the circuit
        (emptyRule

          | staticRoute(RouterFactory.pathPrefix, Page.Root)
            ~> redirectToPage(
              allDocumentsStore.state.allDocuments.headOption match {
                case Some(firstDocument) => Page.DesktopTaskList(documentId = firstDocument.id)
                case None                => Page.DocumentAdministration
              }
            )(Redirect.Replace)

          | staticRuleFromPage(Page.UserProfile, reactAppModule.userProfile.apply)

          | staticRuleFromPage(Page.UserAdministration, reactAppModule.userAdministration.apply)

          | staticRuleFromPage(Page.DocumentAdministration, reactAppModule.documentAdministration.apply)

          | dynamicRuleFromPage(_ / long.caseClass[Page.DesktopTaskList]) { (page, ctl) =>
            reactAppModule.desktopTaskList(page.documentId, ctl)
          }

        // Fallback
        ).notFound(redirectToPage(Page.Root)(Redirect.Replace))
          .onPostRender((prev, cur) =>
            LogExceptionsCallback(dispatcher.dispatch(Action.SetPageLoadingState(isLoading = false))))
          .setTitle(page => s"${page.title} | Task Keeper")
      }
      .renderWith(layout)
  }

  private def layout(routerCtl: RouterCtl[Page], resolution: Resolution[Page])(
      implicit reactAppModule: flux.react.app.Module) = {
    reactAppModule.layout(RouterContext(resolution.page, routerCtl))(
      <.div(^.key := resolution.page.toString, resolution.render()))
  }
}
private[router] object RouterFactory {
  val pathPrefix = "/app/"
}
