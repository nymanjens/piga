package app.flux.react.uielements

import hydro.common.I18n
import hydro.common.time.Clock
import hydro.common.time.DateStringConversions
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
      implicit val i18n: I18n = props.i18n

      val defaultDate =
        props.initialDate.map(_.toLocalDate).getOrElse(props.clock.now.toLocalDate.plusDays(1))
      $.modState(_.copy(inputText = DateStringConversions.dateToHumanCanonicalString(defaultDate)))
    }
    override def render(props: Props, state: State): VdomElement = {
      implicit val i18n: I18n = props.i18n
      implicit val clock: Clock = props.clock

      val now = clock.now.toLocalDate
      val tomorrow = now.plusDays(1)

      val isRemoval = state.inputText.trim.isEmpty
      val parsedDateOpt = if (isRemoval) None else DateStringConversions.stringToDate(state.inputText)
      val isValidDate = parsedDateOpt.exists(d => d.isEqual(tomorrow) || d.isAfter(tomorrow))

      def handleConfirm: Callback = {
        if (isRemoval) props.onConfirm(None)
        else if (isValidDate)
          props.onConfirm(Some(LocalDateTime.of(parsedDateOpt.get, java.time.LocalTime.MIN)))
        else Callback.empty
      }

      def handleKeyDown(e: ReactKeyboardEventFrom[html.Input]): Callback = {
        e.stopPropagation()
        val key = e.key
        if (key == "Enter") {
          e.preventDefault()
          handleConfirm
        } else if (key == "Escape") {
          e.preventDefault()
          props.onCancel
        } else if (key == "ArrowUp" || key == "ArrowDown") {
          e.preventDefault()
          parsedDateOpt.fold(Callback.empty) { parsedDate =>
            val diff = if (key == "ArrowUp") 1 else -1
            val newDate = parsedDate.plusDays(diff)
            $.modState(_.copy(inputText = DateStringConversions.dateToHumanCanonicalString(newDate)))
          }
        } else {
          Callback.empty
        }
      }

      <.div(
        <.div(^.className := "modal-backdrop fade in", ^.zIndex := "1000"),
        <.div(
          ^.className := "bootbox modal fade bootbox-prompt in",
          ^.tabIndex := -1,
          ^.role := "dialog",
          ^.display := "block",
          ^.zIndex := "1001",
          ^.onClick --> props.onCancel,
          <.div(
            ^.className := "modal-dialog",
            ^.onClick ==> ((e: ReactEventFrom[html.Div]) => e.stopPropagationCB),
            <.div(
              ^.className := "modal-content",
              <.div(
                ^.className := "modal-header",
                <.h4(^.className := "modal-title", "Set Delayed Until"),
              ),
              <.div(
                ^.className := "modal-body",
                <.div(
                  ^.className := "bootbox-body",
                  <.form(
                    ^.className := "bootbox-form",
                    <.input(
                      ^.`type` := "text",
                      ^.className := "bootbox-input bootbox-input-text form-control",
                      ^.autoFocus := true,
                      ^.value := state.inputText,
                      ^.onFocus ==> ((e: ReactFocusEventFrom[html.Input]) => Callback { e.target.select() }),
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
                  ),
                  <.div(
                    ^.marginTop := "10px",
                    if (isRemoval) {
                      <.div("This will remove the delayed date")
                    } else {
                      parsedDateOpt match {
                        case Some(date) =>
                          val daysInFuture = ChronoUnit.DAYS.between(now, date)
                          val formattedStr = DateStringConversions.dateToHumanFriendlyString(
                            LocalDateTime.of(date, java.time.LocalTime.MIN),
                            forceIncludeDayOfWeek = true,
                          )

                          <.div(
                            <.div(formattedStr),
                            <.div(s"In: $daysInFuture days"),
                            if (!isValidDate) <.div(^.color := "red", "Date must be at least tomorrow")
                            else EmptyVdom,
                          )
                        case None =>
                          <.div(^.color := "red", "Invalid date format")
                      }
                    },
                  ),
                ),
              ),
              <.div(
                ^.className := "modal-footer",
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
                  ^.disabled := !(isValidDate || isRemoval),
                  ^.onClick --> handleConfirm,
                  "Confirm",
                ),
              ),
            ),
          ),
        ),
      )
    }
  }
}
