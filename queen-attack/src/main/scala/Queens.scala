case class Position(x: Int, y: Int)

case class Queens() {

  private val emptyBoard =
    "_ _ _ _ _ _ _ _\n" +
      "_ _ _ _ _ _ _ _\n" +
      "_ _ _ _ _ _ _ _\n" +
      "_ _ _ _ _ _ _ _\n" +
      "_ _ _ _ _ _ _ _\n" +
      "_ _ _ _ _ _ _ _\n" +
      "_ _ _ _ _ _ _ _\n" +
      "_ _ _ _ _ _ _ _\n"

  // 8 x 8
  def boardString(wOpt: Option[Position], bOpt: Option[Position]): String = (wOpt, bOpt) match {
    case (Some(w), Some(b)) =>
      (for {
        x <- 0 to 7
        y <- 0 to 7
        c = if (Position(x, y) == w) "W" else if (Position(x, y) == b) "B" else "_"
        end = if (y == 7) "\n" else " "
      } yield c + end).mkString
    case _ => emptyBoard
  }

  def canAttack(q1: Position, q2: Position): Boolean =
    q1.x == q2.x ||
      q1.y == q2.y ||
      Math.abs(q1.x - q2.x) == Math.abs(q1.y - q2.y)
}
