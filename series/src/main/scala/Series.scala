import scala.annotation.tailrec

object Series {
  def slices(sliceSize: Int, input: String): List[List[Int]] = {

    @tailrec
    def sliceString(current: String, acc: List[List[Int]]): List[List[Int]] = {
      if(current.length < sliceSize) acc
      else {
        val slice = current.take(sliceSize).map(_.toString.toInt).toList
        sliceString(current.drop(1), acc :+ slice)
      }
    }

    sliceString(input, Nil)
  }
}
