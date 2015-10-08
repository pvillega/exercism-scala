case class PalindromeProducts(start: Int, end: Int) {

  private val palindromes =
    getAllPalindromes
    .groupBy(_._1)
    .map { case (k, v) => (k, v.map(toSortedSet).foldLeft(Set.empty[(Int, Int)])((acc, s) => acc ++ s)) }
    .toList
    .sortBy(_._1)

  val smallest: (Int, Set[(Int, Int)]) = palindromes.head

  val largest: (Int, Set[(Int, Int)]) = palindromes.last

  private def toSortedSet(elem: (Int, Set[(Int, Int)])) = elem._2.map { case (a, b) => if (a > b) (b, a) else (a, b) }

  private def isPalindrome(i: Int): Boolean = i.toString == i.toString.reverse

  private def getAllPalindromes: List[(Int, Set[(Int, Int)])] =
    (for {
      x <- start to end
      y <- start to end
      if isPalindrome(x * y)
    } yield (x * y, Set((x, y)))).toList
}
