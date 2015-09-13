class Sublist {
  def sublist[A](a: List[A], b: List[A]): Sublist.Result =
    if (a == b) Sublist.Equal
    else if (a.containsSlice(b)) Sublist.Superlist
    else if (b.containsSlice(a)) Sublist.Sublist
    else Sublist.Unequal
}

object Sublist {

  sealed trait Result

  case object Equal extends Result

  case object Sublist extends Result

  case object Superlist extends Result

  case object Unequal extends Result

}