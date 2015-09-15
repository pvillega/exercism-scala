case class Robot(bearing: Bearing.Bearing, coordinates: (Int, Int)) {
  def advance(): Robot = Robot(bearing, advanceCoordinates(bearing, coordinates))

  def turnRight(): Robot = Robot(Bearing.apply((bearing.id + 1) % 4), coordinates)

  def turnLeft(): Robot = {
    val next = if(bearing.id == 0) 3 else bearing.id - 1
    Robot(Bearing.apply(next), coordinates)
  }

  def simulate(orders: String): Robot = orders.foldLeft(this){
    (R, move) =>
      if(move == 'A') R.advance()
      else if(move == 'L') R.turnLeft()
      else R.turnRight()
  }

  private def advanceCoordinates(bearing: Bearing.Bearing, coordinates: (Int, Int)): (Int, Int) =
    if (bearing == Bearing.North) (coordinates._1, coordinates._2 + 1)
    else if (bearing == Bearing.South) (coordinates._1, coordinates._2 - 1)
    else if (bearing == Bearing.East) (coordinates._1 + 1, coordinates._2)
    else (coordinates._1 - 1, coordinates._2)
}

object Bearing extends Enumeration {
  type Bearing = Value

  val North = Value(0)
  val East = Value(1)
  val South = Value(2)
  val West = Value(3)

}