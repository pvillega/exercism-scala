case class Ocr(data: String) {

  lazy val convert: String = {
    val rows = data.split( """\n""")
    //iterate over possible rows of digits
    Range.inclusive(0, rows.length - 1, 4).map { i =>
      val slice = rows.slice(i, i + 4)
      splitRowToDigits(slice).map(s => convertDigitToString(findMatch(s))).mkString
    }.mkString(",")
  }

  private def splitRowToDigits(arrays: Array[String]): List[String] =
    Range.inclusive(0, arrays(0).length - 1, 3).map { i =>
      (0 to 3).map(j => arrays(j).substring(i, i + 3)).mkString("\n")
    }.toList

  private def findMatch(digit: String): Int =
    Ocr.templatesByPosition.indexOf(digit)

  private def convertDigitToString(i: Int): String =
    if (i < 0) "?" else i.toString
}

object Ocr {

  protected val templatesByPosition = List(
    List(" _ "
      , "| |"
      , "|_|"
      , "   ").mkString("\n"),
    List("   "
      , "  |"
      , "  |"
      , "   ").mkString("\n"),
    List(" _ "
      , " _|"
      , "|_ "
      , "   ").mkString("\n"),
    List(" _ "
      , " _|"
      , " _|"
      , "   ").mkString("\n"),
    List("   "
      , "|_|"
      , "  |"
      , "   ").mkString("\n"),
    List(" _ "
      , "|_ "
      , " _|"
      , "   ").mkString("\n"),
    List(" _ "
      , "|_ "
      , "|_|"
      , "   ").mkString("\n"),
    List(" _ "
      , "  |"
      , "  |"
      , "   ").mkString("\n"),
    List(" _ "
      , "|_|"
      , "|_|"
      , "   ").mkString("\n"),
    List(" _ "
      , "|_|"
      , " _|"
      , "   ").mkString("\n")
  )

}