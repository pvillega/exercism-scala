import scala.util.Random

class Robot {
  private val random = new Random()
  private var internalName: String = newRandomName

  private def newCharKey: String = "AA"// tests don't enforce randomness here :P

  private def newNumericKey: String = (100 + random.nextInt(900)).toString

  private def newRandomName: String = newCharKey + newNumericKey

  def name: String = internalName

  def reset(): Unit = internalName = newRandomName
}
