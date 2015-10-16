import scala.util.Random

//ascii a -> 97  z -> 122
case class Cipher(cipher: Option[String]) {
  val key: String = cipher match {
    case None => (1 to 100).map(_ => Random.alphanumeric.filter(_.isLower).head).mkString
    case Some(ciph) =>
      if (ciph.isEmpty || ciph.exists(c => c.isDigit || c.isUpper)) throw new IllegalArgumentException()
      else ciph
  }

  def encode(text: String): String = text.zipWithIndex.map { case (c, idx) => encodeChar(c, key(idx)) }.mkString

  def decode(code: String): String = code.zipWithIndex.map { case (c, idx) => decodeChar(c, key(idx)) }.mkString

  private def encodeChar(c: Char, key: Char) = {
    val encoded =  c + key - 'a'
    (if(encoded > 'z') 'a' - 1 + (encoded % 'z') else encoded).toChar
  }

  private def decodeChar(c: Char, key: Char) = (c - key + 'a' % 122).toChar
}

