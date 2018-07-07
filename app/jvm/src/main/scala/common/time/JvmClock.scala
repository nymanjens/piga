package common.time

import java.time.{Instant, LocalDate, LocalTime, ZoneId}

import com.google.inject._

final class JvmClock extends Clock {
  // TODO: Make this configurable
  val zone = ZoneId.of("Europe/Paris")

  override def now = {
    val date = LocalDate.now(zone)
    val time = LocalTime.now(zone)
    LocalDateTime.of(date, time)
  }

  override def nowInstant = Instant.now()
}
