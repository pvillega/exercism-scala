import java.util.{TimeZone, GregorianCalendar}

case class Gigasecond(birth: GregorianCalendar) {

  val date = {
    val calendar = new GregorianCalendar(TimeZone.getTimeZone("GMT"))
    calendar.setTimeInMillis(birth.getTime.getTime + Gigasecond.gigaMilliSecond)
    calendar
  }
}

object Gigasecond {
  val gigaMilliSecond = Math.pow(10,12).toLong
}
