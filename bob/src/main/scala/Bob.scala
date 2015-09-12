
class Bob {
  def hey(statement: String) = answerStatement(statement.trim)

  private def answerStatement(statement: String) =
    if (isSilence(statement)) "Fine. Be that way!"
    else if (isShouting(statement)) "Whoa, chill out!"
    else if (isQuestion(statement)) "Sure."
    else "Whatever."

  private def isSilence(statement: String): Boolean =
    statement.isEmpty

  private def isQuestion(statement: String): Boolean =
    statement.endsWith("?")

  private def isShouting(statement: String): Boolean =
    statement.exists(_.isLetter) && statement.toUpperCase == statement
}
