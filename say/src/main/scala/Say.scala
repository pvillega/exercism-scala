object Say {
  //my original solution was very messy, this one is extremely elegant but based on pre-existing code
  private val constants: Map[Long, String] = Map(1L -> "one", 2L -> "two", 3L -> "three", 4L -> "four", 5L -> "five", 6L -> "six", 7L -> "seven",
    8L -> "eight", 9L -> "nine", 10L -> "ten", 11L -> "eleven", 12L -> "twelve", 13L -> "thirteen", 14L -> "fourteen", 15L -> "fifteen",
    16L -> "sixteen", 17L -> "seveteen", 18L -> "eighteen", 19L -> "nineteen", 20L -> "twenty", 30L -> "thirty", 40L -> "forty", 50L -> "fifty",
    60L -> "sixty", 70L -> "seventy", 80L -> "eighty", 90L -> "ninety")

  private val suffixes = List("", " thousand", " million", " billion", " trillion")

  def inEnglish(i: Long): Option[String] =
    if (i < 0 || i > 999999999999L) None
    else if (i == 0) Some("zero")
    else Some(sayNumber(i))

  private def sayNumber(number: Long) =
    breakIntoTriplets(number)
      .zip(suffixes)
      .flatMap { case (n, suffix) => say(n % 1000).map(_ + suffix) }
      .reverse
      .mkString(" ")

  private def breakIntoTriplets(n: Long) = Stream.iterate(n)(_ / 1000)

  private def say(number: Long): Option[String] =
    if (number == 0) None
    else if (number < 20) Some(constants(number))
    else if (number < 100) Some(constants(number / 10 * 10) + say(number % 10).map("-" + _).getOrElse(""))
    else Some(constants(number / 100) + " hundred" + say(number % 100).map(" " + _).getOrElse(""))
}
