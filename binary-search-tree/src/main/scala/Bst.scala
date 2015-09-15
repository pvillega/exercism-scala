import scala.math.Ordering.Implicits._

case class Bst[A: Ordering](value: A, left: Option[Bst[A]] = None, right: Option[Bst[A]] = None) {

  def insert(n: A): Bst[A] =
    if (n <= value) {
      val newLeft = if (left.isEmpty) Bst(n) else left.get.insert(n)
      this.copy(left = Some(newLeft))
    } else if (n > value) {
      val newRight = if (right.isEmpty) Bst(n) else right.get.insert(n)
      this.copy(right = Some(newRight))
    } else this

}

object Bst {

  def fromList[A: Ordering](list: List[A]): Bst[A] =
    list.tail.foldLeft(Bst(list.head)) { (bst, n) => bst.insert(n) }

  def toList[A](bst: Bst[A]): List[A] = {
    def traverseBst(treeOpt: Option[Bst[A]]): List[A] = treeOpt match {
      case None => Nil
      case Some(tree) =>
        val l = traverseBst(tree.left)
        val r = traverseBst(tree.right)
        l ::: (tree.value :: r)
    }

    traverseBst(Option(bst))
  }
}
