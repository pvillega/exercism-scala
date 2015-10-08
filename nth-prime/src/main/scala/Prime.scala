import scala.annotation.tailrec

object Prime {

  private var cache = List[Int](2)

  def nth(i: Int): Int = {
    if (cache.length < i) {
      cache = increaseCacheTo(i, cache)
    }
    cache(i - 1)
  }

  @tailrec
  private def increaseCacheTo(i: Int, vector: List[Int]): List[Int] =
    if (vector.length > i) vector
    else increaseCacheTo(i, vector :+ findNextPrime(vector.last + 1))

  @tailrec
  private def findNextPrime(start: Int): Int =
    if (isPrime(start)) start
    else findNextPrime(start + 1)

  private def isPrime(n: Int) =
    if (n < 2) false
    else !(2 to Math.sqrt(n).toInt).exists(n % _ == 0)
}
