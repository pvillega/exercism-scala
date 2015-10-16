import scala.annotation.tailrec

object Sieve {
  //this is bloody inefficient, following readme by heart :)
  def primesUpTo(i: Int): List[Int] = {

    @tailrec
    def sieve(range: List[Int], acc: List[Int]): List[Int] = range match {
      case Nil => acc
      case h :: tail =>sieve(tail.filterNot( _ % h == 0), acc :+ h)
    }

    sieve((2 to i).toList, Nil)
  }

}
