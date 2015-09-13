import java.util.{Calendar, GregorianCalendar}

class Meetup(month: Int, year: Int) {
  private val calendarMonth = month - 1
  private val teenthDays = List(13, 14, 15, 16, 17, 18, 19)

  def teenth(day: Meetup.Day): GregorianCalendar =
    (for {
      week <- 1 to 5
      possibleMatch = selectNthDayOfMonth(day, week)
      if teenthDays.contains(possibleMatch.get(Calendar.DAY_OF_MONTH))
    } yield possibleMatch).head

  def first(day: Meetup.Day): GregorianCalendar = selectNthDayOfMonth(day, 1)

  def second(day: Meetup.Day): GregorianCalendar = selectNthDayOfMonth(day, 2)

  def third(day: Meetup.Day): GregorianCalendar = selectNthDayOfMonth(day, 3)

  def fourth(day: Meetup.Day): GregorianCalendar = selectNthDayOfMonth(day, 4)

  def last(day: Meetup.Day): GregorianCalendar = selectNthDayOfMonth(day, -1)

  private def selectNthDayOfMonth(day: Meetup.Day, nth: Int) = {
    val calendar = new GregorianCalendar(year, calendarMonth, 1)
    calendar.set(Calendar.DAY_OF_WEEK, day.calendarDay)
    calendar.set(Calendar.DAY_OF_WEEK_IN_MONTH, nth)
    calendar
  }

}

object Meetup {

  sealed trait Day {
    def calendarDay: Int
  }

  case object Mon extends Day {
    override def calendarDay: Int = Calendar.MONDAY
  }

  case object Tue extends Day {
    override def calendarDay: Int = Calendar.TUESDAY
  }

  case object Wed extends Day {
    override def calendarDay: Int = Calendar.WEDNESDAY
  }

  case object Thu extends Day {
    override def calendarDay: Int = Calendar.THURSDAY
  }

  case object Fri extends Day {
    override def calendarDay: Int = Calendar.FRIDAY
  }

  case object Sat extends Day {
    override def calendarDay: Int = Calendar.SATURDAY
  }

  case object Sun extends Day {
    override def calendarDay: Int = Calendar.SUNDAY
  }

  def apply(month: Int, year: Int) = new Meetup(month, year)

}
