case class Garden(names: List[String], chain: String) {

  private def getPlants(rows: Array[String], child: Int): List[Plant.Plant] = {
    val pos0 = child * 2
    val pos1 = pos0 + 1

    if(rows(0).length < pos1) Nil
    else List(rows(0).charAt(pos0),rows(0).charAt(pos1),rows(1).charAt(pos0),rows(1).charAt(pos1))
      .map(_.toString)
      .map(Plant.withName)
  }

  private def processGarden(): Map[String, List[Plant.Plant]] = {
    val rows = chain.split( """\n""")
    val children = names.sorted

    (0 until children.length)
      .map(i => children(i) -> getPlants(rows, i))
      .toMap
  }

  private val map = processGarden()

  def getPlants(name: String): List[Plant.Plant] = map.getOrElse(name, Nil)
}

object Garden {
  private val kids = List(
    "Alice", "Bob", "Charlie", "David",
    "Eve", "Fred", "Ginny", "Harriet",
    "Ileana", "Joseph", "Kincaid", "Larry")

  def defaultGarden(chain: String): Garden = Garden(kids, chain)
}

object Plant extends Enumeration {
  type Plant = Value
  val Violets = Value("V")
  val Radishes = Value("R")
  val Grass = Value("G")
  val Clover = Value("C")
}
