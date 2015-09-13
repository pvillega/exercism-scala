case class RomanNumeral(number: Int) {

  @scala.annotation.tailrec
  private def toRomanString(n: Int, acc: String): String =
    RomanNumeral.sortedValuesbyKey.find(_ <= n) match {
    case Some(digit) => toRomanString(n - digit, acc + RomanNumeral.values.getOrElse(digit, ""))
    case None => acc
  }

  val value: String = toRomanString(number, "")
}

object RomanNumeral {

  private val values: Map[Int, String] = Map(
    1 -> "I", 4 -> "IV",
    5 -> "V", 9 -> "IX",
    10 -> "X", 40 -> "XL",
    50 -> "L", 90 -> "XC",
    100 -> "C", 400 -> "CD",
    500 -> "D", 900 -> "CM",
    1000 -> "M")

  //we will use this sorted list of keys to find matches to roman characters of the given decimal number
  private val sortedValuesbyKey: List[Int] = values.keys.toList.sortWith((a,b) => a > b)
}