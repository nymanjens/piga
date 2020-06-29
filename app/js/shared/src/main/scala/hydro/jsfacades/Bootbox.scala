package hydro.jsfacades

import scala.concurrent.Future
import scala.concurrent.Promise
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

/** Facade on top of bootbox.js */
object Bootbox {

  def alert(str: String): Unit = {
    RawJsBootbox.alert(str)
  }

  def prompt(title: String, value: String, animate: Boolean): Future[Option[String]] = {
    val resultPromise: Promise[Option[String]] = Promise()
    val callback: js.Function1[String, Unit] = response => {
      resultPromise.success(Option(response))
    }

    RawJsBootbox.prompt(
      js.Dynamic.literal(
        title = title,
        value = value,
        callback = callback,
        animate = animate,
      ))

    resultPromise.future
  }

  // Using global instead of import because bootbox seems to rely on JQuery and bootstrap.js being
  // present in the global scope
  @JSGlobal("bootbox")
  @js.native
  object RawJsBootbox extends js.Object {
    def alert(str: String): Unit = js.native
    def prompt(parameters: js.Object): Unit = js.native
  }
}
