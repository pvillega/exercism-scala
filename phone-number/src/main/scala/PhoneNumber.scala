class PhoneNumber(input: String) {

  private val invalidNumber = "0000000000"

  val number: String = input.filter(_.isDigit) match {
    case num if num.length == 10 => num
    case num if num.length == 11 && num.head == '1' => num.tail
    case _ => invalidNumber
  }

  val areaCode: String = number.take(3).mkString

  val localNumber: String = s"${number.substring(3,6)}-${number.substring(6)}"

  override def toString = s"($areaCode) $localNumber"
}
