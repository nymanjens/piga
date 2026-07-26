package hydro.common.time

import java.time.DateTimeException
import java.time.LocalDate
import java.time.LocalTime
import java.time.Month

import scala.collection.immutable.Seq

object TimeUtils {

  def requireStartOfMonth(date: LocalDate): Unit = {
    require(date.getDayOfMonth == 1, s"Date $date should be at the first day of the month.")
  }

  def allMonths: Seq[Month] = Month.values().toList
}
