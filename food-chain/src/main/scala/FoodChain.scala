object FoodChain {

  private val end = "\nI don't know why she swallowed the fly. Perhaps she'll die."

  private val fly = ("fly", "")
  private val spider = ("spider", "It wriggled and jiggled and tickled inside her.")
  private val bird = ("bird", "How absurd to swallow a bird!")
  private val cat = ("cat", "Imagine that, to swallow a cat!")
  private val dog = ("dog", "What a hog, to swallow a dog!")
  private val goat = ("goat", "Just opened her throat and swallowed a goat!")
  private val cow = ("cow", "I don't know how she swallowed a cow!")
  private val horse = ("horse", "She's dead, of course!\n")

  private val items = List(fly, spider, bird, cat, dog, goat, cow, horse)

  //I still dislike song questions
  def song: String = items.zipWithIndex.map { case (item, i) => section(item, i) }.mkString("\n")

  private def section(element: (String, String), position: Int): String = {
    // skip fly and horse
    val body = if (position > 0 && position < items.length - 1) {
      (0 to position - 1).map { i =>
        val index = position - i
        val spiderAddon = if(index - 1 == 1) " that wriggled and jiggled and tickled inside her" else ""
        s"She swallowed the ${items(index)._1} to catch the ${items(index - 1)._1}$spiderAddon."
      }.mkString(s"\n${element._2}\n", "\n", end)
    } else if (position == 0) {
      end
    } else s"\n${element._2}" //horse

    s"I know an old lady who swallowed a ${element._1}.$body\n"
  }

}
