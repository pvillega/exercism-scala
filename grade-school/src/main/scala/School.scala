import scala.collection.mutable
import scala.collection.immutable

class School {
  private val internalDb = mutable.Map.empty[Int, List[String]]

  def add(name: String, grade: Int): Unit = {
    val students = internalDb.getOrElseUpdate(grade, List()) :+ name
    internalDb.put(grade, students)
  }

  def grade(grade: Int): Seq[String] = List(internalDb.getOrElse(grade, Nil) : _*)

  def db: immutable.Map[Int, Seq[String]] = internalDb.toMap

  def sorted: immutable.Map[Int, Seq[String]] =
    immutable.TreeMap(internalDb.mapValues(_.sorted).toSeq :_*)

}
