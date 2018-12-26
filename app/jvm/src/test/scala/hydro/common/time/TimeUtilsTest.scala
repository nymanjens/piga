package hydro.common.time

import java.time.LocalDate
import java.time.Month._

import org.specs2.mutable._

class TimeUtilsTest extends Specification {

  "parseDateString" in {
    TimeUtils.parseDateString("1992-07-22") mustEqual LocalDateTimes.createDateTime(1992, JULY, 22)
    TimeUtils.parseDateString("2001-7-3") mustEqual LocalDateTimes.createDateTime(2001, JULY, 3)
    TimeUtils.parseDateString("1992-07-33") must throwA[IllegalArgumentException]
    TimeUtils.parseDateString("1992-0722") must throwA[IllegalArgumentException]
    TimeUtils.parseDateString("1992-07-22-") must throwA[IllegalArgumentException]
    TimeUtils.parseDateString("1992-07-dd") must throwA[IllegalArgumentException]
    TimeUtils.parseDateString("19920722") must throwA[IllegalArgumentException]
  }
}
