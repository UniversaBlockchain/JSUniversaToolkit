package tools

import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

object TimeUtils {

  private val timestampFormat = "HH:mm:ss.SSS"

  def ts: String = {
    val date = new Date
    val sdf = new SimpleDateFormat(timestampFormat)
    sdf.format(date)
  }
}
