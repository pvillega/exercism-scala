case class Matrix(content: String) {
  //vector of rows (each row a vector of Int)
  private val simplifiedMatrix: Vector[Vector[Int]] =
    content
      .split("""\n""")
      .map(convertRows)
      .toVector

  private def convertRows(s: String): Vector[Int] =
    s.split(" ").map(_.toInt).toVector

  def rows(n: Int): Vector[Int] = simplifiedMatrix(n)

  def cols(n: Int): Vector[Int] = simplifiedMatrix.map(v => v(n))

  override def hashCode(): Int = simplifiedMatrix.hashCode()

  override def equals(obj: scala.Any): Boolean =
    obj match {
      case m: Matrix =>
        simplifiedMatrix == m.simplifiedMatrix
      case _ => false
    }

}
