object PigLatin {
  private val vowelSounds = Set('a', 'e', 'i', 'o', 'u')
  //order matters in cluster tokens
  private val consonantClusters = List("sch", "thr", "squ", "qu", "th", "ch")

  def translate(sentence: String): String =
    sentence.split(" ").map(convertWord).mkString(" ")

  private def convertWord(word: String): String =
    if (rule1Check(word)) ayizeWord(word)
    else if (clusterCheck(word)) ayizeWord(clusterTransform(word))
    else ayizeWord(rule2Transform(word))


  private def rule1Check(w: String): Boolean = vowelSounds.contains(w.head)

  private def ayizeWord(w: String): String = w + "ay"

  private def rule2Transform(w: String): String = w.tail + w.head

  private def clusterCheck(w: String): Boolean = consonantClusters.exists(w.startsWith)

  private def clusterTransform(w: String): String = consonantClusters.filter(w.startsWith).map(s => w.drop(s.length) + s).head
}
