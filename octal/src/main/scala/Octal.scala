object Octal {
  private val validChars = List('0', '1', '2', '3', '4', '5', '6', '7')

  def intToOctal(i: Int): String = {
    def convertToOctal(n: Int, acc: String): String =
      if(n < 8) (acc + n).reverse
      else convertToOctal(n/8, acc + (n % 8))

    convertToOctal(i, "")
  }

  def octalToInt(s: String): Int =
    if (invalidOctalNumber(s)) throw new scala.IllegalArgumentException()
    else convertOctalToInt(s)

  private def invalidOctalNumber(s: String): Boolean =
    s.isEmpty || !s.forall(validChars.contains(_))

  private def convertOctalToInt(s: String): Int =
    s.foldRight((0, 0)) { (c, acc) =>
      val index = acc._1
      val sum = acc._2
      val x = c.toString.toInt

      (index + 1, sum + x * Math.pow(8, index).toInt)
    }._2

}
