package shared

object SharedTimeUtils {
  val SecondMS: Double = 1000.0
  val MinuteMS: Double = 60.0 * SecondMS
  val HourMS: Double = 60.0 * MinuteMS
  val DayMS: Double = 24.0 * HourMS
  val WeekMS: Double = 7.0 * DayMS
  val MonthMS: Double = 30.0 * WeekMS

  def timeControlParts(timecontrol: String) = {
    val tc = timecontrol.replaceAll(" ", "")
    tc.split("\\+")
  }

  def timeControlToTimeMs(timecontrol: String): Double = {
    try {
      val ms = timeControlParts(timecontrol)(0).toDouble * MinuteMS
      return ms
    } catch { case e: Throwable => return 3.0 * MinuteMS }
  }

  def timeControlToIncrementMs(timecontrol: String): Double = {
    try {
      val ms = timeControlParts(timecontrol)(1).toDouble * SecondMS
      return ms
    } catch { case e: Throwable => return 2000.0 }
  }
}
