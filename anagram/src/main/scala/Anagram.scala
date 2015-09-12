class Anagram(source: String) {

  private val sourceFreqMap: Map[Char, Int] = convertToLowerCaseFrequencyMap(source)

  private def notEqualIgnoreCaseToSourceWord(word: String): Boolean = !word.equalsIgnoreCase(source)

  private def isAnagramIgnoringCase(word: String): Boolean =
    sourceFreqMap == convertToLowerCaseFrequencyMap(word)

  private def convertToLowerCaseFrequencyMap(word: String): Map[Char, Int] =
    word.toLowerCase.groupBy(c => c).mapValues(_.length)

  def matches(seq: Seq[String]): Seq[String] =
    seq.filter(s => notEqualIgnoreCaseToSourceWord(s) && isAnagramIgnoringCase(s))
}
