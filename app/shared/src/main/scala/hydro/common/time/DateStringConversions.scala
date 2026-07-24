package hydro.common.time

import java.lang.Math.abs
import java.time.DayOfWeek._
import java.time.DayOfWeek
import java.time.LocalDate
import java.time.Month
import java.time.Month._
import scala.util.Try

import hydro.common.I18n

object DateStringConversions {
  // Note: Cannot use DateTimeFormatter as it isn't supported by scala.js

  val monthToMessageKey: Map[Month, String] = Map(
    JANUARY -> "app.date.month.jan.abbrev",
    FEBRUARY -> "app.date.month.feb.abbrev",
    MARCH -> "app.date.month.mar.abbrev",
    APRIL -> "app.date.month.apr.abbrev",
    MAY -> "app.date.month.may.abbrev",
    JUNE -> "app.date.month.jun.abbrev",
    JULY -> "app.date.month.jul.abbrev",
    AUGUST -> "app.date.month.aug.abbrev",
    SEPTEMBER -> "app.date.month.sep.abbrev",
    OCTOBER -> "app.date.month.oct.abbrev",
    NOVEMBER -> "app.date.month.nov.abbrev",
    DECEMBER -> "app.date.month.dec.abbrev",
  )

  val dayOfWeekToMessageKey: Map[DayOfWeek, String] = Map(
    MONDAY -> "app.date.dayofweek.mon.abbrev",
    TUESDAY -> "app.date.dayofweek.tue.abbrev",
    WEDNESDAY -> "app.date.dayofweek.wed.abbrev",
    THURSDAY -> "app.date.dayofweek.thu.abbrev",
    FRIDAY -> "app.date.dayofweek.fri.abbrev",
    SATURDAY -> "app.date.dayofweek.sat.abbrev",
    SUNDAY -> "app.date.dayofweek.sun.abbrev",
  )

  private val DayMonthRegex = """^(\d{1,2})\s+([a-zA-Z]+)$""".r
  private val MonthDayRegex = """^([a-zA-Z]+)\s+(\d{1,2})$""".r
  private val DayMonthYearRegex = """^(\d{1,2})\s+([a-zA-Z]+)\s+(\d{4})$""".r
  private val MonthDayYearRegex = """^([a-zA-Z]+)\s+(\d{1,2})\s+(\d{4})$""".r
  private val IsoRegex = """^(\d{4})-(\d{1,2})-(\d{1,2})$""".r

  def dateToHumanReadableString(dateTime: LocalDateTime, forceIncludeDayOfWeek: Boolean = false)(implicit
      i18n: I18n,
      clock: Clock,
  ): String = {
    val now = clock.now.toLocalDate
    val date = dateTime.toLocalDate

    val yearString = date.getYear.toString takeRight 2
    val dayMonthString = {
      val monthString = formatMonth(date)
      s"${date.getDayOfMonth} $monthString"
    }

    var includeDayOfWeek = forceIncludeDayOfWeek
    val baseString =
      if (date.getYear == now.getYear) {
        val dayDifference = abs(now.getDayOfYear - date.getDayOfYear)

        if (date.getDayOfYear == now.getDayOfYear) {
          i18n("app.today")
        } else if (date.getDayOfYear == now.getDayOfYear - 1) {
          i18n("app.yesterday")
        } else if (date.getDayOfYear == now.getDayOfYear + 1) {
          i18n("app.tomorrow")
        } else {
          if (dayDifference < 7) {
            includeDayOfWeek = true
          }
          dayMonthString
        }
      } else {
        s"$dayMonthString '$yearString"
      }

    if (includeDayOfWeek) {
      s"${formatDayOfWeek(date)}, $baseString"
    } else {
      baseString
    }
  }

  def stringToDate(input: String)(implicit i18n: I18n, clock: Clock): Option[LocalDate] = {
    val now = clock.now.toLocalDate
    val monthNames: Map[String, Month] = Month
      .values()
      .flatMap { m =>
        val engName = m.name().toLowerCase
        val engAbbrev = engName.take(3)
        val localAbbrev = i18n(monthToMessageKey(m)).toLowerCase
        Seq(engName -> m, engAbbrev -> m, localAbbrev -> m)
      }
      .toMap

    def resolveDate(year: Int, monthStr: String, dayStr: String): Option[LocalDate] = {
      for {
        month <- monthNames.get(monthStr.toLowerCase)
        day <- Try(dayStr.toInt).toOption
        if day >= 1 && day <= month.length(java.time.Year.isLeap(year.toLong))
      } yield LocalDate.of(year, month, day)
    }

    def resolveDateCurrentOrNextYear(monthStr: String, dayStr: String): Option[LocalDate] = {
      resolveDate(now.getYear, monthStr, dayStr).map { date =>
        if (date.isBefore(now)) {
          // If the date has already passed this year, assume next year
          date.plusYears(1)
        } else {
          date
        }
      }
    }

    input.trim.toLowerCase match {
      case IsoRegex(y, m, d) =>
        Try(LocalDate.of(y.toInt, m.toInt, d.toInt)).toOption
      case DayMonthRegex(d, m) =>
        resolveDateCurrentOrNextYear(m, d)
      case MonthDayRegex(m, d) =>
        resolveDateCurrentOrNextYear(m, d)
      case DayMonthYearRegex(d, m, y) =>
        resolveDate(y.toInt, m, d)
      case MonthDayYearRegex(m, d, y) =>
        resolveDate(y.toInt, m, d)
      case _ =>
        None
    }
  }

  private def formatMonth(date: LocalDate)(implicit i18n: I18n): String = {
    i18n(monthToMessageKey(date.getMonth))
  }

  private def formatDayOfWeek(date: LocalDate)(implicit i18n: I18n): String = {
    i18n(dayOfWeekToMessageKey(date.getDayOfWeek))
  }
}
