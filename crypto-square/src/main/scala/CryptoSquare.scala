case class CryptoSquare() {
  private val noEmptySpace = ""
  private val oneEmptySpace = " "


  def normalizePlaintext(text: String): String =
    text
      .filter(_.isLetterOrDigit)
      .map(_.toLower)

  def squareSize(text: String): Int =
    if (text.isEmpty) 0
    else Math.sqrt(text.length).ceil.toInt

  def plaintextSegments(text: String): List[String] = {
    val norm = normalizePlaintext(text)
    val square = squareSize(norm)

    normalise(norm, square).split(" ").toList
  }

  def ciphertext(text: String): String = {
    val segments = plaintextSegments(text)
    subcipherAt(segments).mkString
  }

  def normalizedCiphertext(text: String): String = {
    val segments = plaintextSegments(text)
    subcipherAt(segments).mkString(" ")
  }

  private def normalise(s: String, square: Int): String =
    s.foldLeft((0, "")) {
      (acc, c) =>
        val s = if (acc._1 > 0 && acc._1 % square == 0) oneEmptySpace else noEmptySpace
        (acc._1 + 1, acc._2 + s + c)
    }._2

  private def subcipherAt(list: List[String]) = {
    val size = list.head.length
    list.map(_.padTo(size, " "))
      .transpose
      .map(_.mkString)
      .map(_.replaceAll(oneEmptySpace, noEmptySpace))
  }

}
