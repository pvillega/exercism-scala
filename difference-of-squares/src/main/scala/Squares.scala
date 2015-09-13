case class Squares() {
  def squareOfSums(value: Int): Int = Math.pow((1 to value).sum, 2).toInt

  def sumOfSquares(value: Int): Int = (1 to value).map(i => i * i).sum

  def difference(value: Int): Int = squareOfSums(value) - sumOfSquares(value)
}
