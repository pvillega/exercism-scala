object Series {
  def digits(s: String): List[Int] = s.map(_.toString.toInt).toList

  def slices(n: Int, s: String): List[List[Int]] = {
    val list = digits(s)
    (0 to list.size)
      .map { i => list.slice(i, i + n) }
      .filter(_.size == n) // slice can include smaller lists
      .toList
  }

  def largestProduct(n: Int, s: String): Int =
    slices(n, s).map(_.product) match {
      case Nil => 1
      case l => l.max
    }
}
