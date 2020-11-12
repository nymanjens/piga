package app.flux.react.uielements

import scala.concurrent.Future
import scala.concurrent.Promise
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import org.scalajs.dom
import org.scalajs.dom.raw.HTMLInputElement
import hydro.flux.react.HydroReactComponent
import hydro.jsfacades.Bootbox
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.MountedImpure
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.vdom.TagMod
import japgolly.scalajs.react.Ref.ToScalaComponent
import japgolly.scalajs.react.internal.Box
import org.scalajs.dom.html

import scala.collection.immutable.Seq
import scala.scalajs.js

object SelectPrompt {
  def choose(title: String, optionsIdToName: Map[Long, String]): Future[Option[Long]] = {
    val result = Bootbox.prompt(title, value = "", animate = false, selectValue = false)
    Future.successful(None)
  }
}
