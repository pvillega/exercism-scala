class Phrase(sentence: String) {
  private val spaces = """\s+""".stripMargin
  private val punctuation = "[:!&@$%^.,]"
  private val oneEmptySpace = " "

  private def cleanSentence(phrase: String): String =
    phrase
      .map(_.toLower)
      .replaceAll(punctuation, oneEmptySpace)
      .replaceAll(spaces, oneEmptySpace)

  private def countWords(phrase: String): Map[String, Int] =
    phrase
      .split(spaces)
      .groupBy(s => s)
      .map { case (k, v) => (k, v.length) }

  lazy val wordCount: Map[String, Int] = countWords(cleanSentence(sentence))
}
