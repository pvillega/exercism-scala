object PythagoreanTriplet {

  //a**2 + b**2 = c**2
  def isPythagorean(t: (Int, Int, Int)): Boolean = {
    def checkIsPitagorean(a: Int, b: Int, c: Int) = (a*a) + (b*b) == (c*c)

    checkIsPitagorean(t._1, t._2, t._3) || checkIsPitagorean(t._2, t._3, t._1) || checkIsPitagorean(t._3, t._1, t._2)
  }

  def pythagoreanTriplets(start: Int, end: Int): Seq[(Int, Int, Int)] =
    for {
      a <- start to end
      b <- a to end
      c <- b to end
      if isPythagorean(a, b, c)
    } yield (a, b, c)
}
