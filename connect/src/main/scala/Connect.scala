import scala.annotation.tailrec

case class Connect(board: List[String]) {

  def result: Option[Color.Player] =
    if (whiteHasAPath()) Some(Color.White)
    else if (blackHasAPath()) Some(Color.Black)
    else None

  //top to bottom
  private def whiteHasAPath(): Boolean = hasAPath(Color.White, board)

  // left to right
  private def blackHasAPath(): Boolean = hasAPath(Color.Black, board.transpose.map(_.mkString))

  //always check top to bottom for given player, assume perfectly aligned board
  private def hasAPath(player: Color.Player, grid: List[String]): Boolean = {
    existsPathBetweenRows(0, grid.length - 1, findCellsOfPlayer(player, grid))
  }

  private def findCellsOfPlayer(player: Color.Player, grid: List[String]): Set[Cell] =
    (for {
      y <- grid.indices
      x <- grid(y).indices
      if grid(y)(x).toString == player
    } yield Cell(x + y, y)).toSet //move cell x value to simulate rhombus

  private def existsPathBetweenRows(startRow: Int, endRow: Int, cells: Set[Cell]): Boolean = {
    val startCells = cells.filter(_.y == startRow)
    val endCells = cells.filter(_.y == endRow)
    val allPaths = findLongestIndependentPaths(cells)

    allPaths.exists(path => path.exists(startCells.contains) && path.exists(endCells.contains))
  }

  private def findLongestIndependentPaths(cells: Set[Cell]): Set[Set[Cell]] = {

    @tailrec
    def mergePaths(paths: Set[Set[Cell]]): Set[Set[Cell]] = {

      def canMerge(l1: Set[Cell], l2: Set[Cell]): Boolean = {
        val verticalPath = l1.exists(c1 => l2.exists(c2 => c2.x == c1.x))
        val horizontalPath = l1.exists(c1 => l2.exists(c2 => c2.y == c1.y && Math.abs(c1.x - c2.x) == 1))
        val diagonalPath = l1.exists(c1 => l2.exists(c2 => (c2.y == c1.y - 1 || c2.y == c1.y + 1) && (c2.x == c1.x - 1 || c2.x == c1.x + 1)))

        verticalPath || horizontalPath || diagonalPath
      }

      if(paths.isEmpty) paths
      else {
        val toMerge = paths.head
        val rest = paths.tail

        if (rest.exists(canMerge(toMerge, _))) {
          val merged = rest.map(set => if (canMerge(toMerge, set)) toMerge ++ set else set)
          mergePaths(merged)
        } else paths
      }
    }
    //we use sets to remove duplicates automatically
    mergePaths(cells.map(Set(_)))
  }
}

case class Cell(x: Int, y: Int)

object Color {
  type Player = String
  //left to right
  val Black: Player = "X"
  //top to bottom
  val White: Player = "O"
  val Empty = "."
}