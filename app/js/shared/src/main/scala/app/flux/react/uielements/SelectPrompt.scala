package app.flux.react.uielements

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.async.Async.async
import scala.async.Async.await
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
import org.scalajs.dom.raw.Element
import org.scalajs.dom.raw.Event
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.raw.HTMLFormElement

import scala.collection.immutable.ListMap
import scala.collection.immutable.Seq
import scala.scalajs.js

object SelectPrompt {
  def choose(title: String, optionsIdToName: ListMap[Long, String]): Future[Option[Long]] = {
    val result = Bootbox.prompt(title, value = "", animate = false, selectValue = false)

    val form = dom.document.getElementsByClassName("bootbox-form").apply(0)
    val optionsDiv = dom.document.createElement("div")
    optionsDiv.id = "SelectPrompt-options"
    form.appendChild(optionsDiv)

    val diaglogHandler = new DialogHandler(options = optionsIdToName.values.toVector, optionsDiv = optionsDiv)

    diaglogHandler.renderOptionsList(Matcher.noopMatcher)

    val promptInput =
      dom.document.getElementsByClassName("bootbox-input-text").apply(0).asInstanceOf[HTMLInputElement]
    addChangeListeners(promptInput, newValue => diaglogHandler.renderOptionsList(new Matcher(newValue)))

    async {
      for {
        searchString <- await(result)
        matcher <- Some(new Matcher(searchString))
        (optionId, optionName) <- optionsIdToName.toVector.sortBy(pair => matcher.rank(pair._2)).lastOption
        if matcher.isMatch(optionName)
      } yield optionId
    }
  }

  private def addChangeListeners(input: HTMLInputElement, listener: String => Unit): Unit = {
    def getEventValue(event: Event): String =
      event.target.asInstanceOf[js.Dynamic].value.asInstanceOf[String]

    input.onchange = event => listener(getEventValue(event))
    input.onkeydown = event => listener(getEventValue(event))
    input.onpaste = event => listener(getEventValue(event))
    input.oninput = event => listener(getEventValue(event))
    input.onchange = event => listener(getEventValue(event))
  }

  private class Matcher(val searchString: String) {
    def isMatch(option: String): Boolean = {
      rank(option) > 0
    }

    def rank(option: String): Int = {
      if (option.startsWith(searchString)) {
        4
      } else if (option.toLowerCase.startsWith(searchString.toLowerCase())) {
        3
      } else if (option.contains(searchString)) {
        2
      } else if (option.toLowerCase.contains(searchString.toLowerCase())) {
        1
      } else {
        0
      }
    }
  }
  private object Matcher {
    val noopMatcher: Matcher = new Matcher("")
  }

  private class DialogHandler(options: Seq[String], optionsDiv: Element) {
    private var lastSearchValue: String = null

    def renderOptionsList(matcher: Matcher): Unit = {
      if (lastSearchValue != matcher.searchString) {
        lastSearchValue = matcher.searchString
        optionsDiv.innerHTML = "" +
          "<ul>" +
          (
            for ((option, index) <- options.sortBy(o => -matcher.rank(o)).zipWithIndex)
              yield {
                val className =
                  if (!matcher.isMatch(option)) "no-match"
                  else if (index == 0) "match selected"
                  else "match"
                s"<li class='$className'>$option</li>"
              }
          ).mkString("") +
          "</ul>"
      }
    }
  }
}
