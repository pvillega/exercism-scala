object Grains {
  private val validRange = 0 to 63
  private val mapGrains: Map[Int, BigInt] = validRange.map(i => i + 1 -> BigInt(2).pow(i)).toMap

  val total: BigInt = mapGrains.values.sum

  def square(n: Int): BigInt = mapGrains(n)

}
