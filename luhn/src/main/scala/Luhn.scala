case class Luhn(value: Long) {
  private val listNumbers = value.toString.toCharArray.map(_.toString.toInt).toList

  val checkDigit: Long = listNumbers.last

  val addends: List[Long] =
    listNumbers
      .reverse
      .zipWithIndex
      .map {
        case (n, idx) =>
          if (idx % 2 != 0) {
            val r = n * 2L
            if (r > 10) r - 9 else r
          } else n
      }
      .reverse

  val checksum: Long = addends.sum % 10

  val isValid: Boolean = checksum == 0

  def create: Long = {
     val digit = Luhn((listNumbers :+ 0).mkString.toLong).checksum
    (listNumbers :+ (10 - digit) % 10).mkString.toLong
  }
}
