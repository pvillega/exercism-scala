case class PrimeFactors() {


  def primeFactors(n: Long): List[Long] ={
    @scala.annotation.tailrec
    def factors(n: Long, acc: List[Long]): List[Long] =
      findNextPrimeIsDivisibleBy(n) match {
        case Some(i) => factors(n/i, i :: acc)
        case None =>  n :: acc
      }

    if(n < 2) Nil
    else factors(n, Nil).sorted
  }

  private def findNextPrimeIsDivisibleBy(n: Long): Option[Long] =
    (2L to Math.sqrt(n).toInt).find(i => n % i == 0 && PrimeFactors.isPrime(i) )

}

object PrimeFactors {

  def isPrime(n: Long): Boolean =
    if (n < 2L) false
    else !(2L to Math.sqrt(n).toInt).exists(n % _ == 0)

}
