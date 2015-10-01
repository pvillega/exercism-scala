object WordProblem {

  // adding 'raised to the' breaks [a-z ]{4,} as term for ops in operationRegex
  private val plus = "plus"
  private val minus = "minus"
  private val multiplied = "multiplied by"
  private val divided = "divided by"
  private val raised = "raised to the"
  private val ops = s"$plus|$minus|$multiplied|$divided|$raised"

  private val questionRegex = """What is (-?\d+ .+ -?\d+).""".r
  private val operationRegex = s"""(.+) ($ops) (.+)""".r
  private val constantRegex = """(-?\d+)""".r

  def apply(s: String): Option[Int] = {
    Some(convertToOps(s))
  }

  private def convertToOps(s: String): Int = s match {
    case questionRegex(operations) =>
      convertToOps(operations)
    case operationRegex(x, op, y) if op == plus =>
      convertToOps(x) + convertToOps(y)
    case operationRegex(x, op, y) if op == minus =>
      convertToOps(x) - convertToOps(y)
    case operationRegex(x, op, y) if op == multiplied =>
      convertToOps(x) * convertToOps(y)
    case operationRegex(x, op, y) if op == divided =>
      convertToOps(x) / convertToOps(y)
    case operationRegex(x, op, y) if op == raised =>
      Math.pow(convertToOps(x), convertToOps(y)).toInt
    case constantRegex(x) => x.toInt
  }

}
