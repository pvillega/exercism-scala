case class Dna() {
  def toRna(chain: String): String = chain.map(Dna.transcriptionMap)
}

object Dna {
  private val transcriptionMap: Map[Char, Char] = Map('G' -> 'C', 'C' -> 'G', 'T' -> 'A', 'A' -> 'U')
}
