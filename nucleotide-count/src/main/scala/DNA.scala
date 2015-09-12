class DNA(dna: String) {

  private val validNucleotids = List('A', 'T', 'C', 'G')

  private def isValidNucleotid(n: Char) = validNucleotids.contains(n)

  val nucleotideCounts: Map[Char, Int] =
    if (!dna.forall(isValidNucleotid)) throw new IllegalArgumentException(s"DNA contains invalid nucleotids: $dna")
    else validNucleotids
            .map(base => (base, count(base)))
            .toMap

  def count(base: Char): Int =
    if (isValidNucleotid(base)) dna.count(_ == base)
    else throw new IllegalArgumentException(s"Invalid nucleotid $base")

}
