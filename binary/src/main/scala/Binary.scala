case class Binary(number: String) {

  @scala.annotation.tailrec
  private def transform(chain: String, pos: Int, acc: Int): Int =
    if (chain.isEmpty) acc
    else if (chain.head != '0' && chain.head != '1') 0
    else transform(chain.tail, pos + 1, acc + calculateBinaryValue(chain, pos))

  private def calculateBinaryValue(chain: String, pos: Int): Int =
    if (chain.head == '0') 0
    else Math.pow(2, pos).toInt

  val toDecimal: Int = transform(number.reverse, 0, 0)
}
