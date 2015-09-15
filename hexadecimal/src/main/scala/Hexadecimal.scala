object Hexadecimal {

  private val mapChars = Map('A' -> 10, 'B' -> 11, 'C' -> 12, 'D' -> 13, 'E' -> 14, 'F' -> 15)

  def hexToInt(hex: String): Int =
    if (invalidString(hex)) 0
    else parseHex(hex.toUpperCase)

  private def invalidString(hex: String) =
    hex.isEmpty || hex.exists(c => c.isLetter && mapChars.get(c.toUpper).isEmpty)

  private def parseHex(hex: String): Int = {
    //convert string to array of integers were pos 0 is lower value
    val array = hex.reverse.toCharArray.map(c => if (c.isLetter) mapChars(c) else c.toString.toInt)
    // set value along array
    array.zipWithIndex.map { case (value, pos) => Math.pow(16, pos).toInt * value }.sum
  }
}
