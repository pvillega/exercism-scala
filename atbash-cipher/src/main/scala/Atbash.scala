case class Atbash() {
  private val spaces = """\s+""".stripMargin
  private val punctuation = "[:!&@$%^.,]"
  private val noEmptySpace = ""
  private val oneEmptySpace = " "

  //a -> 97 to z -> 122
  def encode(msg: String): String =
    cleanSentence(msg)
      .map(encodeChar)
      .foldLeft((0,"")){
      (acc, c) =>
        val s = if(acc._1 > 0 && acc._1 % 5 == 0) oneEmptySpace else noEmptySpace
        (acc._1 + 1, acc._2 + s +  c)
    }._2

  private def cleanSentence(phrase: String): String =
    phrase
      .map(_.toLower)
      .replaceAll(punctuation, noEmptySpace)
      .replaceAll(spaces, noEmptySpace)

  private def encodeChar(c: Char): Char =
    if(c.isDigit) c
    else ((110 - c) + 109).toChar
}
