package app.flux.react.uielements

import hydro.common.CollectionUtils
import hydro.common.GuavaReplacement.Splitter

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

    val diaglogHandler = new DialogHandler(optionsIdToName, optionsDiv)

    diaglogHandler.renderOptionsList(Matcher.noopMatcher, selectedIndex = 0)

    val promptInput =
      dom.document.getElementsByClassName("bootbox-input-text").apply(0).asInstanceOf[HTMLInputElement]
    addChangeListeners(
      promptInput,
      onAnyChange = newValue => diaglogHandler.setSearchString(newValue),
      onArrowUp = () => diaglogHandler.incrementSelectedIndex(diff = -1),
      onArrowDown = () => diaglogHandler.incrementSelectedIndex(diff = +1),
    )

    async {
      for {
        searchString <- await(result)
        optionId <- diaglogHandler.lastSelectedOptionId
      } yield optionId
    }
  }

  private def addChangeListeners(
      input: HTMLInputElement,
      onAnyChange: String => Unit,
      onArrowUp: () => Unit,
      onArrowDown: () => Unit,
  ): Unit = {
    def getEventValue(event: Event): String =
      event.target.asInstanceOf[js.Dynamic].value.asInstanceOf[String]

    input.onchange = event => onAnyChange(getEventValue(event))
    input.onpaste = event => onAnyChange(getEventValue(event))
    input.oninput = event => onAnyChange(getEventValue(event))
    input.onchange = event => onAnyChange(getEventValue(event))

    input.onkeydown = event => {
      event.keyCode match {
        case 38 =>
          onArrowUp()
          event.preventDefault()
        case 40 =>
          onArrowDown()
          event.preventDefault()
        case _ => onAnyChange(getEventValue(event))
      }
    }
  }

  private def max(ints: Int*): Int = {
    Seq(ints: _*).max
  }

  private class Matcher(val searchString: String) {
    def isMatch(option: String): Boolean = {
      rank(option) > 0
    }

    def rank(option: String): Int = {
      def scoreWithExtraPointIfCaseMatches(option: String, searchString: String, points: Int)(
          matchingFunc: (String, String) => Boolean
      ): Int = {
        if (matchingFunc(option, searchString)) points + 1
        else if (matchingFunc(option.toLowerCase, searchString.toLowerCase)) points
        else 0
      }
      max(
        scoreWithExtraPointIfCaseMatches(option, searchString, 20000)(_ startsWith _),
        scoreWithExtraPointIfCaseMatches(option, searchString, 10000)(_ contains _),
        // Tokenized match
        {
          (
            for (searchPart <- Splitter.on(' ').split(searchString)) yield {
              val bestOptionScore = (
                for (optionPart <- Splitter.on(' ').split(option))
                  yield max(
                    scoreWithExtraPointIfCaseMatches(optionPart, searchPart, 300)(_ == _),
                    scoreWithExtraPointIfCaseMatches(optionPart, searchPart, 200)(_ startsWith _),
                    scoreWithExtraPointIfCaseMatches(optionPart, searchPart, 100)(_ contains _),
                  )
              ).max

              // If one search token doesn't match, it means there is no match
              if (bestOptionScore == 0) Int.MinValue else bestOptionScore
            }
          ).sum
        },
      )
    }

    def ordering: Ordering[String] = {
      Ordering.by(string => -rank(string))
    }
  }
  private object Matcher {
    val noopMatcher: Matcher = new Matcher("")
  }

  private class DialogHandler(optionsIdToName: ListMap[Long, String], optionsDiv: Element) {
    private var lastSearchString: String = _
    private var lastSelectedIndex: Int = -99

    def renderOptionsList(matcher: Matcher, selectedIndex: Int): Unit = {
      if (lastSearchString != matcher.searchString || lastSelectedIndex != selectedIndex) {
        val selectedIndexWithinBounds = {
          val matchedOptions = options.count(matcher.isMatch)
          if (selectedIndex < 0) 0
          else if (selectedIndex >= matchedOptions) matchedOptions - 1
          else selectedIndex
        }

        lastSearchString = matcher.searchString
        lastSelectedIndex = selectedIndexWithinBounds

        optionsDiv.innerHTML = "" +
          "<ul>" +
          (
            for ((option, index) <- options.sorted(matcher.ordering).zipWithIndex)
              yield {
                val className =
                  if (!matcher.isMatch(option)) "no-match"
                  else if (index == selectedIndexWithinBounds) "match selected"
                  else "match"
                s"<li class='$className'>$option</li>"
              }
          ).mkString("") +
          "</ul>"
      }
    }

    def setSearchString(searchString: String): Unit = {
      renderOptionsList(
        matcher = new Matcher(searchString),
        selectedIndex =
          if (lastSearchString == searchString) lastSelectedIndex
          else 0,
      )
    }

    def incrementSelectedIndex(diff: Int): Unit = {
      renderOptionsList(matcher = new Matcher(lastSearchString), selectedIndex = lastSelectedIndex + diff)
    }

    def lastSelectedOptionId: Option[Long] = {
      val matcher = new Matcher(lastSearchString)
      val maybePair = CollectionUtils.maybeGet(
        optionsIdToName.toVector.sortBy(_._2)(matcher.ordering).filter(pair => matcher.isMatch(pair._2)),
        index = lastSelectedIndex,
      )
      maybePair.map(_._1)
    }

    private def options: Seq[String] = {
      optionsIdToName.values.toVector
    }
  }
}
