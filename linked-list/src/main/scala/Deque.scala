case class Deque[A]() {

  // From Readme :
  // The simplest data structure for this will have a pair of linked lists for the front and back, where
  // iteration order is front ++ reverse back.

  private var front: List[A] = List()
  private var back: List[A] = List()

  //back
  def push(a: A): Unit = {
    back = a :: back
  }

  def pop: Option[A] = back match {
    case Nil =>
      back = front.reverse
      front = Nil
      pop
    case h :: tail =>
      back = tail
      Some(h)
  }

  //front
  def unshift(a: A): Unit = {
    front = a :: front
  }

  def shift: Option[A] = front match {
    case Nil =>
      front = back.reverse
      back = Nil
      shift
    case h :: tail =>
      front = tail
      Some(h)
  }
}
