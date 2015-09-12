object Hamming {

  private def calculateHammingDifference(source: String, other: String): Int = {
    def areCharsEqual(a: Char, b: Char): Int = if(a == b) 0 else 1

    @scala.annotation.tailrec
    def tailRecCalculation(sa: String, sb: String, acc: Int): Int =
      if(sa.length == 0) acc
      else tailRecCalculation(sa.tail, sb.tail, acc + areCharsEqual(sa.head, sb.head))

    tailRecCalculation(source, other, 0)
  }

  def compute(seqA: String, seqB: String) =
    if(seqA.length != seqB.length) throw new IllegalArgumentException("Strands of different length")
    else calculateHammingDifference(seqA, seqB)
}
