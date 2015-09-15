case class Raindrops() {
  def convert(n: Int): String = {
    val p3 = if (n % 3 == 0) "Pling" else ""
    val p5 = if (n % 5 == 0) "Plang" else ""
    val p7 = if (n % 7 == 0) "Plong" else ""

    val s = s"$p3$p5$p7"

    if(s.isEmpty) n.toString
    else s
  }

}
