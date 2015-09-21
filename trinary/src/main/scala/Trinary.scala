object Trinary {
  private val validChars = List('0', '1', '2')

  def intToTrinary(i: Int): String = {
    def convert(n: Int, acc: String): String =
      if (n < 3) (acc + n).reverse
      else convert(n / 3, acc + (n % 3))

    convert(i, "")
  }

  def trinaryToInt(s: String): Int =
    if (invalidTrinaryNumber(s)) throw new scala.IllegalArgumentException()
    else convertToInt(s)

  private def invalidTrinaryNumber(s: String): Boolean =
    s.isEmpty || !s.forall(validChars.contains(_))

  private def convertToInt(s: String): Int =
    s.foldRight((0, 0)) { (c, acc) =>
      val index = acc._1
      val sum = acc._2
      val x = c.toString.toInt

      (index + 1, sum + x * Math.pow(3, index).toInt)
    }._2
}
