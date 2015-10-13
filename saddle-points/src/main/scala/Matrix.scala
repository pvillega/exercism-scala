case class Matrix(data: List[List[Int]]) {


  // It's called a "saddle point" because it is greater than or equal to
  // every element in its row and the less than or equal to every element in
  // its column.
  val saddlePoints: Set[(Int, Int)] =
    (for {
      x <- data.indices
      y <- data(x).indices
      if data(x).max == data(x)(y) && findMinimumOfColumn(y) == data(x)(y)
    } yield (x, y)).toSet

  private def findMinimumOfColumn(col: Int): Int = data.map(l => l(col)).min
}
