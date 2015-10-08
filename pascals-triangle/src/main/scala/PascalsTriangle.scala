import scala.annotation.tailrec

object PascalsTriangle {

  /*
  each row has as many elements as row number
  row position value = (row - 1)(pos - 1) + (row - 1)(pos)
    1
    1 1
    1 2 1
    1 3 3 1
    1 4 6 4 1
   */

  def triangle(rows: Int): List[List[Int]] = calculateTriangle(2, rows, List(List(1)))

  @tailrec
  private def calculateTriangle(currentRow: Int, maxRow: Int, triangle: List[List[Int]]): List[List[Int]] =
    if(currentRow > maxRow) triangle.reverse
    else calculateTriangle(currentRow + 1, maxRow, calculateRow(currentRow, triangle.head) :: triangle)

  private def calculateRow(row: Int, previousRow: List[Int]): List[Int] =
    (0 until row).map(i => if(i == 0) 1 else if(i == previousRow.size) 1 else previousRow(i - 1) + previousRow(i)).toList
}
