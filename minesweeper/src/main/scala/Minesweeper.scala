object Minesweeper {

  def annotate(list: List[String]): List[String] = {
    val array = convertToArray(list)
    val populated = populateMineCount(array)
    backToListString(populated)
  }

  private def convertToArray(list: List[String]): Array[Array[Char]] =
    list.map(_.toCharArray).toArray

  private def backToListString(array: Array[Array[Char]]): List[String] =
    array.map(_.mkString).toList

  private def populateMineCount(array: Array[Array[Char]]): Array[Array[Char]] = {
    for {
      y <- 0 to array.length
      if y < array.length
      x <- 0 to array(y).length
      if x < array(y).length
      c = array(y)(x)
    } {
      val count = countAdjancentMines(array, x, y)
      val replacement = if(c == ' ' && count > 0) count.toString.head else c
      array(y)(x) = replacement
    }

    array
  }

  private def countAdjancentMines(array: Array[Array[Char]], x: Int, y: Int): Int =
    (for {
      i <- y - 1 to y + 1
      if i >= 0 && i < array.length
      j <- x - 1 to x + 1
      if j >= 0 && j < array(i).length
    } yield if (array(i)(j) == '*') 1 else 0).sum

}
