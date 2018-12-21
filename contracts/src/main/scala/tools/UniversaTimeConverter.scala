package tools

import java.time.{Instant, LocalDateTime, ZoneId}
import scala.language.implicitConversions
import scala.scalajs.js

object UniversaTimeConverter {

  private val getCurrentZoneId = ZoneId.systemDefault()

  implicit def jsDateAsLocalDateTime(d: js.Date): LocalDateTime = {
    LocalDateTime.ofInstant(Instant.ofEpochMilli(d.getMilliseconds()), getCurrentZoneId)
  }

  implicit def toJSDate(d: LocalDateTime): js.Date = {
    new js.Date(d.atZone(getCurrentZoneId).toInstant.toEpochMilli)
  }
}
