class Year(year: Int) {

//  on every year that is evenly divisible by 4
//  except every year that is evenly divisible by 100
//  unless the year is also evenly divisible by 400

  private def divisibleBy(divisor: Int)(i: Int): Boolean = i % divisor == 0
  private def divisibleBy4(i: Int): Boolean = divisibleBy(4)(i)
  private def divisibleBy100(i: Int): Boolean = divisibleBy(100)(i)
  private def divisibleBy400(i: Int): Boolean = divisibleBy(400)(i)

  def isLeap: Boolean = divisibleBy4(year) && !(divisibleBy100(year) && !divisibleBy400(year))
}

object Year {
  def apply(year: Int) = new Year(year)
}