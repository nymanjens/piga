package app.flux.react.uielements

import hydro.common.Formatting
import hydro.common.I18n
import hydro.common.time.Clock
import hydro.common.time.DateParser
import hydro.common.time.LocalDateTime
import hydro.flux.react.HydroReactComponent
import hydro.flux.react.ReactVdomUtils.^^
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html

import java.time.LocalDate
import java.time.temporal.ChronoUnit

object DelayedTaskDatePicker extends HydroReactComponent {

  // **************** API ****************//
  def apply(
      initialDate: Option[LocalDateTime],
      onConfirm: Option[LocalDateTime] => Callback,
      onCancel: Callback,
  )(implicit i18n: I18n, clock: Clock): VdomElement = {
    component(Props(initialDate, onConfirm, onCancel, i18n, clock))
  }

  // **************** Implementation of HydroReactComponent methods ****************//
  override protected val config =
    ComponentConfig(backendConstructor = new Backend(_), initialState = State(""))

  // **************** Implementation of HydroReactComponent types ****************//
  protected case class Props(
      initialDate: Option[LocalDateTime],
      onConfirm: Option[LocalDateTime] => Callback,
      onCancel: Callback,
      i18n: I18n,
      clock: Clock,
  )

  protected case class State(inputText: String)

  protected class Backend($ : BackendScope[Props, State]) extends BackendBase($) with WillMount {
    override def willMount(props: Props, state: State): Callback = {
      val defaultDate =
        props.initialDate.map(_.toLocalDate).getOrElse(props.clock.now.toLocalDate.plusDays(1))
      val initialInput =
        s"${defaultDate.getDayOfMonth} ${props.i18n(Formatting.monthToMessageKey(defaultDate.getMonth))} ${defaultDate.getYear}"
      $.modState(_.copy(inputText = initialInput))
    }
    override def render(props: Props, state: State): VdomElement = {
      implicit val i18n: I18n = props.i18n
      implicit val clock: Clock = props.clock

      val now = clock.now.toLocalDate
      val tomorrow = now.plusDays(1)
      val parsedDateOpt = DateParser.parseDate(state.inputText, now)

      val isValidDate = parsedDateOpt.exists(d => d.isEqual(tomorrow) || d.isAfter(tomorrow))

      def handleKeyDown(e: ReactKeyboardEventFrom[html.Input]): Callback = {
        e.stopPropagation()
        val key = e.key
        if (key == "Enter") {
          e.preventDefault()
          if (isValidDate) {
            props.onConfirm(Some(LocalDateTime.of(parsedDateOpt.get, java.time.LocalTime.MIN)))
          } else {
            Callback.empty
          }
        } else if (key == "Escape") {
          e.preventDefault()
          props.onCancel
        } else if (key == "ArrowUp" || key == "ArrowDown") {
          e.preventDefault()
          parsedDateOpt.fold(Callback.empty) { parsedDate =>
            val diff = if (key == "ArrowUp") 1 else -1
            val newDate = parsedDate.plusDays(diff)
            val newInput =
              s"${newDate.getDayOfMonth} ${i18n(Formatting.monthToMessageKey(newDate.getMonth))} ${newDate.getYear}"
            $.modState(_.copy(inputText = newInput))
          }
        } else {
          Callback.empty
        }
      }

      <.div(
        ^.className := "delayed-task-date-picker-overlay",
        ^.position := "fixed",
        ^.top := "0",
        ^.left := "0",
        ^.width := "100%",
        ^.height := "100%",
        ^.backgroundColor := "rgba(0, 0, 0, 0.5)",
        ^.zIndex := "1000",
        ^.onClick --> props.onCancel,
        <.div(
          ^.className := "delayed-task-date-picker-modal",
          ^.position := "absolute",
          ^.top := "50%",
          ^.left := "50%",
          ^.transform := "translate(-50%, -50%)",
          ^.backgroundColor := "white",
          ^.padding := "20px",
          ^.borderRadius := "5px",
          ^.boxShadow := "0 4px 8px rgba(0, 0, 0, 0.2)",
          ^.onClick ==> ((e: ReactEventFrom[html.Div]) => e.stopPropagationCB),
          <.h3("Set Delayed Until"),
          <.input(
            ^.`type` := "text",
            ^.className := "form-control",
            ^.autoFocus := true,
            ^.value := state.inputText,
            ^.onChange ==> ((e: ReactEventFrom[html.Input]) => {
              val text = e.target.value
              $.modState(_.copy(inputText = text))
            }),
            ^.onKeyDown ==> handleKeyDown,
            ^.onKeyPress ==> ((e: ReactKeyboardEventFrom[html.Input]) => e.stopPropagationCB),
            ^.onPaste ==> ((e: ReactClipboardEventFrom[html.Input]) => e.stopPropagationCB),
            ^.onCopy ==> ((e: ReactClipboardEventFrom[html.Input]) => e.stopPropagationCB),
            ^.onCut ==> ((e: ReactClipboardEventFrom[html.Input]) => e.stopPropagationCB),
          ),
          <.div(
            ^.marginTop := "10px",
            parsedDateOpt match {
              case Some(date) =>
                val daysInFuture = ChronoUnit.DAYS.between(now, date)
                val dayOfWeekStr = i18n(Formatting.dayOfWeekToMessageKey(date.getDayOfWeek))
                val formattedStr = Formatting.formatDate(LocalDateTime.of(date, java.time.LocalTime.MIN))

                <.div(
                  <.div(s"Date: $formattedStr"),
                  <.div(s"Day of week: $dayOfWeekStr"),
                  <.div(s"In: $daysInFuture days"),
                  if (!isValidDate) <.div(^.color := "red", "Date must be at least tomorrow") else EmptyVdom,
                )
              case None =>
                <.div(^.color := "red", "Invalid date format")
            },
          ),
          <.div(
            ^.marginTop := "15px",
            if (props.initialDate.isDefined) {
              <.button(
                ^.className := "btn btn-danger",
                ^.marginRight := "10px",
                ^.onClick --> props.onConfirm(None),
                "Remove Delayed Date (Move to Unsorted)",
              )
            } else EmptyVdom,
            <.button(
              ^.className := "btn btn-primary",
              ^.disabled := !isValidDate,
              ^.onClick --> (if (isValidDate)
                               props.onConfirm(
                                 Some(LocalDateTime.of(parsedDateOpt.get, java.time.LocalTime.MIN))
                               )
                             else Callback.empty),
              "Confirm",
            ),
          ),
        ),
      )
    }
  }
}
