case class Clock(h: Int, m: Int) {
  val (realMinute, realHour) = setRealTime()

  private def setRealTime(): (Int, Int) = {
    var accMin = m % 60
    var accHour = (h + m / 60) % 24

    if(accMin < 0) {
      accMin = 60 + accMin
      accHour = accHour - 1
    }

    if(accHour < 0) {
      accHour = 24 + (accHour % 24)
    }

    (accMin, accHour)
  }

  override def toString: String = f"$realHour%02d:$realMinute%02d"

  override def hashCode(): Int = super.hashCode()

  override def equals(obj: scala.Any): Boolean =
    obj match {
      case c: Clock => c.realMinute == realMinute && c.realHour == realHour
      case _ => false
    }

  def +(other: Clock): Clock = Clock(h + other.h, m + other.m)

  def -(other: Clock): Clock = Clock(h - other.h, m - other.m)
}

object Clock {
  def apply(m: Int): Clock = new Clock(0, m)
}