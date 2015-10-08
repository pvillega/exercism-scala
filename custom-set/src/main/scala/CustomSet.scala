class CustomSet[A](protected val elems: List[A])

object CustomSet {

  def fromList[A](list: List[A]): CustomSet[A] = new CustomSet(list)

  def toList[A](set: CustomSet[A]): List[A] = set.elems

  def empty[A](set: CustomSet[A]): Boolean = set.elems.isEmpty

  def singleton[A](set: CustomSet[A]): Boolean = set.elems.size == 1

  def size[A](set: CustomSet[A]): Int = set.elems.size

  def member[A](set: CustomSet[A], a: A): Boolean = set.elems.contains(a)

  def insert[A](set: CustomSet[A], a: A): CustomSet[A] =
    if (member(set, a)) set
    else new CustomSet(a :: set.elems)

  def delete[A](set: CustomSet[A], a: A): CustomSet[A] =
    if (member(set, a)) new CustomSet(set.elems.filter(_ != a))
    else set

  def union[A](a: CustomSet[A], b: CustomSet[A]): CustomSet[A] =
    new CustomSet(listNotCommon(a, b) ::: b.elems)

  def difference[A](a: CustomSet[A], b: CustomSet[A]): CustomSet[A] =
    new CustomSet(listNotCommon(a, b) ::: listNotCommon(b, a))

  def intersection[A](a: CustomSet[A], b: CustomSet[A]): CustomSet[A] =
    new CustomSet(a.elems.filter(member(b, _)))

  def isSubsetOf[A](a: CustomSet[A], b: CustomSet[A]): Boolean =
    a.elems.forall(member(b, _))

  def isDisjointFrom[A](a: CustomSet[A], b: CustomSet[A]): Boolean =
    size(intersection(a, b)) == 0

  private def listNotCommon[A](a: CustomSet[A], b: CustomSet[A]) =
    a.elems.filterNot(member(b, _))
}
