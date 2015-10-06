object House {

  private val house = ("house that Jack built.", "")
  private val malt = ("malt", "lay in")
  private val rat = ("rat", "ate")
  private val cat = ("cat", "killed")
  private val dog = ("dog", "worried")
  private val cow = ("cow with the crumpled horn", "tossed")
  private val maiden = ("maiden all forlorn", "milked")
  private val man = ("man all tattered and torn", "kissed")
  private val priest = ("priest all shaven and shorn", "married")
  private val rooster = ("rooster that crowed in the morn", "woke")
  private val farmer = ("farmer sowing his corn", "kept")
  private val horse = ("horse and the hound and the horn", "belonged to")

  private val items = List(house, malt, rat, cat, dog, cow, maiden, man, priest, rooster, farmer, horse)

  def rhyme: String = items.zipWithIndex.map { case (item, i) => s"This is the ${section(item, i)}" }.mkString("", "\n", "\n")

  private def section(element: (String, String), position: Int): String = {
    val recurse = if (position > 0) s"that ${element._2} the ${section(items(position - 1), position - 1)}" else ""
    s"${element._1}\n$recurse"
  }
}