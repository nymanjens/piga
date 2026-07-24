package hydro.common.time

import hydro.common.Formatting
import hydro.common.I18n
import java.time.LocalDate
import java.time.Month
import scala.util.Try

object DateParser {

  private val DayMonthRegex = """^(\d{1,2})\s+([a-zA-Z]+)$""".r
  private val MonthDayRegex = """^([a-zA-Z]+)\s+(\d{1,2})$""".r
  private val DayMonthYearRegex = """^(\d{1,2})\s+([a-zA-Z]+)\s+(\d{4})$""".r
  private val MonthDayYearRegex = """^([a-zA-Z]+)\s+(\d{1,2})\s+(\d{4})$""".r
  private val IsoRegex = """^(\d{4})-(\d{1,2})-(\d{1,2})$""".r

  def parseDate(input: String, now: LocalDate)(implicit i18n: I18n): Option[LocalDate] = {
    val monthNames: Map[String, Month] = Month
      .values()
      .flatMap { m =>
        val engName = m.name().toLowerCase
        val engAbbrev = engName.take(3)
        val localAbbrev = i18n(Formatting.monthToMessageKey(m)).toLowerCase
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
}
