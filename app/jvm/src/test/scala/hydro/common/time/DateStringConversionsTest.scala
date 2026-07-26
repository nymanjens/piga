package hydro.common.time

import java.time.Month._
import java.time.LocalDate

import app.common.testing._
import hydro.common.testing._
import com.google.inject._
import hydro.common.time.LocalDateTimes.createDateTime
import org.junit.runner._
import org.specs2.runner._

@RunWith(classOf[JUnitRunner])
class DateStringConversionsTest extends HookedSpecification {

  @Inject implicit private val fakeClock: FakeClock = null
  @Inject implicit private val fakeI18n: FakeI18n = null

  override def before() = {
    Guice.createInjector(new TestModule).injectMembers(this)
    setFakeI18nMappings()
    fakeClock.setNow(createDateTime(2010, APRIL, 4))
  }

  "dateToHumanFriendlyString()" in {
    DateStringConversions.dateToHumanFriendlyString(createDateTime(2010, MARCH, 31)) mustEqual "Wed, 31 Mar"
    DateStringConversions.dateToHumanFriendlyString(createDateTime(2010, APRIL, 1)) mustEqual "Thu, 1 Apr"
    DateStringConversions.dateToHumanFriendlyString(createDateTime(2010, APRIL, 2)) mustEqual "Fri, 2 Apr"
    DateStringConversions.dateToHumanFriendlyString(createDateTime(2010, APRIL, 3)) mustEqual "Yesterday"
    DateStringConversions.dateToHumanFriendlyString(createDateTime(2010, APRIL, 4)) mustEqual "Today"
    DateStringConversions.dateToHumanFriendlyString(createDateTime(2010, APRIL, 5)) mustEqual "Tomorrow"
    DateStringConversions.dateToHumanFriendlyString(createDateTime(2010, APRIL, 6)) mustEqual "Tue, 6 Apr"
    DateStringConversions.dateToHumanFriendlyString(createDateTime(2010, APRIL, 7)) mustEqual "Wed, 7 Apr"

    DateStringConversions.dateToHumanFriendlyString(createDateTime(2010, JANUARY, 1)) mustEqual "1 Jan"
    DateStringConversions.dateToHumanFriendlyString(createDateTime(2009, DECEMBER, 31)) mustEqual "31 Dec '09"

    DateStringConversions.dateToHumanFriendlyString(createDateTime(2012, JANUARY, 12)) mustEqual "12 Jan '12"
    DateStringConversions.dateToHumanFriendlyString(createDateTime(2012, FEBRUARY, 12)) mustEqual "12 Feb '12"
    DateStringConversions.dateToHumanFriendlyString(createDateTime(2012, MARCH, 12)) mustEqual "12 Mar '12"
    DateStringConversions.dateToHumanFriendlyString(createDateTime(2012, APRIL, 12)) mustEqual "12 Apr '12"
    DateStringConversions.dateToHumanFriendlyString(createDateTime(2012, MAY, 12)) mustEqual "12 May '12"
    DateStringConversions.dateToHumanFriendlyString(createDateTime(2012, JUNE, 12)) mustEqual "12 June '12"
    DateStringConversions.dateToHumanFriendlyString(createDateTime(2012, JULY, 12)) mustEqual "12 July '12"
    DateStringConversions.dateToHumanFriendlyString(createDateTime(2012, AUGUST, 12)) mustEqual "12 Aug '12"
    DateStringConversions.dateToHumanFriendlyString(
      createDateTime(2012, SEPTEMBER, 12)
    ) mustEqual "12 Sept '12"
    DateStringConversions.dateToHumanFriendlyString(createDateTime(2012, OCTOBER, 12)) mustEqual "12 Oct '12"
    DateStringConversions.dateToHumanFriendlyString(createDateTime(2012, NOVEMBER, 12)) mustEqual "12 Nov '12"
    DateStringConversions.dateToHumanFriendlyString(createDateTime(2012, DECEMBER, 12)) mustEqual "12 Dec '12"
  }

  "stringToDate()" in {
    // Explicit year
    DateStringConversions.stringToDate("2010-04-05") mustEqual Some(LocalDate.of(2010, APRIL, 5))
    DateStringConversions.stringToDate("5 Apr 2010") mustEqual Some(LocalDate.of(2010, APRIL, 5))
    DateStringConversions.stringToDate("Apr 5 2010") mustEqual Some(LocalDate.of(2010, APRIL, 5))

    // Implicit year - current year
    DateStringConversions.stringToDate("Apr 5") mustEqual Some(LocalDate.of(2010, APRIL, 5))
    DateStringConversions.stringToDate("5 Apr") mustEqual Some(LocalDate.of(2010, APRIL, 5))

    // Past date -> falls back to current year (which is actually the past).
    DateStringConversions.stringToDate("Feb 1") mustEqual Some(LocalDate.of(2010, FEBRUARY, 1))

    // Test the new logic: previous year less than 3 months ago.
    fakeClock.setNow(createDateTime(2010, JANUARY, 15))
    // Nov 1 is previous year, less than 3 months ago.
    DateStringConversions.stringToDate("Nov 1") mustEqual Some(LocalDate.of(2009, NOVEMBER, 1))
    // Oct 1 is previous year, MORE than 3 months ago. Will fall back to current year.
    DateStringConversions.stringToDate("Oct 1") mustEqual Some(LocalDate.of(2010, OCTOBER, 1))

    // Test the new logic: next year less than 3 months in the future.
    fakeClock.setNow(createDateTime(2010, DECEMBER, 15))
    // Feb 1 is next year, less than 3 months in the future.
    DateStringConversions.stringToDate("Feb 1") mustEqual Some(LocalDate.of(2011, FEBRUARY, 1))
    // Apr 1 is next year, MORE than 3 months in the future. Will fall back to current year.
    DateStringConversions.stringToDate("Apr 1") mustEqual Some(LocalDate.of(2010, APRIL, 1))
  }

  private def setFakeI18nMappings(): Unit = {
    fakeI18n.setMappings(
      "app.today" -> "Today",
      "app.yesterday" -> "Yesterday",
      "app.tomorrow" -> "Tomorrow",
      "app.date.month.jan.abbrev" -> "Jan",
      "app.date.month.feb.abbrev" -> "Feb",
      "app.date.month.mar.abbrev" -> "Mar",
      "app.date.month.apr.abbrev" -> "Apr",
      "app.date.month.may.abbrev" -> "May",
      "app.date.month.jun.abbrev" -> "June",
      "app.date.month.jul.abbrev" -> "July",
      "app.date.month.aug.abbrev" -> "Aug",
      "app.date.month.sep.abbrev" -> "Sept",
      "app.date.month.oct.abbrev" -> "Oct",
      "app.date.month.nov.abbrev" -> "Nov",
      "app.date.month.dec.abbrev" -> "Dec",
      "app.date.dayofweek.mon.abbrev" -> "Mon",
      "app.date.dayofweek.tue.abbrev" -> "Tue",
      "app.date.dayofweek.wed.abbrev" -> "Wed",
      "app.date.dayofweek.thu.abbrev" -> "Thu",
      "app.date.dayofweek.fri.abbrev" -> "Fri",
      "app.date.dayofweek.sat.abbrev" -> "Sat",
      "app.date.dayofweek.sun.abbrev" -> "Sun",
    )
  }
}
