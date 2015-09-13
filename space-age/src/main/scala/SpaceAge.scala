import scala.math.BigDecimal.RoundingMode

class SpaceAge(age: Long) {
  private val earthYear = 31557600

  def seconds: Long = age

  def onEarth: Double = roundAge(earthYear)

  def onMercury: Double = roundAge(earthYear * 0.2408467)

  def onVenus: Double = roundAge(earthYear * 0.61519726)

  def onMars: Double = roundAge(earthYear * 1.8808158)

  def onJupiter: Double = roundAge(earthYear * 11.862615)

  def onSaturn: Double = roundAge(earthYear * 29.447498)

  def onUranus: Double = roundAge(earthYear * 84.016846)

  def onNeptune: Double = roundAge(earthYear * 164.79132)

  private def roundAge(ratio: Double) = BigDecimal.decimal(age / ratio).setScale(2, RoundingMode.HALF_UP).toDouble
}

object SpaceAge {
  def apply(ageInSeconds: Long) = new SpaceAge(ageInSeconds)
}