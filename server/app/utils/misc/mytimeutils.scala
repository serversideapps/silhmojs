package utils.misc

import org.apache.commons.lang.time.DateFormatUtils._

object MyTimeUtils {
  def formatDateAsTime(d: java.util.Date) = format(d, "yyyy.MM.dd HH:mm:ss")
  def formatDateAsDateOnly(d: java.util.Date) = format(d, "yyyy.MM.dd")
  def formatDateAsTimeOnly(d: java.util.Date) = format(d, "HH:mm:ss")
}