object SecretHandshake {

  def handshake(n: Int): List[String] =
    handshake(Integer.toBinaryString(n))

  def handshake(n: String): List[String] =
    if (n.length == 5 && n(0) == '1') mapToActions(n.drop(1)).reverse
    else mapToActions(n)

  private def mapToActions(n: String) =
    n.toList
      .reverse
      .zipWithIndex
      .map { case (c, idx) => if (c == '1') mapIdxToAction(idx) else "" }
      .filterNot(_ == "")

  private def mapIdxToAction(idx: Int) =
    if (idx == 0) "wink"
    else if (idx == 1) "double blink"
    else if (idx == 2) "close your eyes"
    else if (idx == 3) "jump"
    else ""
}
