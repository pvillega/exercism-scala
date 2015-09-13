case class Triangle(a: Int, b: Int, c: Int) {
  val triangleType: TriangleType.TriangleType =
    if (!isLogical) TriangleType.Illogical
    else if (hasAllEquals) TriangleType.Equilateral
    else if (has2Equals) TriangleType.Isosceles
    else TriangleType.Scalene

  private def hasAllEquals: Boolean = a == b && b == c

  private def has2Equals: Boolean = a == b || a == c || b == c

  private def isLogical: Boolean =
    (a > 0 && b > 0 && c > 0) &&
    (a + b > c && b + c > a && a + c > b)
}

object TriangleType {

  sealed trait TriangleType

  case object Equilateral extends TriangleType

  case object Isosceles extends TriangleType

  case object Scalene extends TriangleType

  case object Illogical extends TriangleType

}