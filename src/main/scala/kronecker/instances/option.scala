package kronecker
package instances

// None, Some(0), Some(1), ...
case class FOption[A](ev: Finite[A]) extends Finite[Option[A]] {
  val size: Z = ev.size + 1
  def get(index: Z): Option[Option[A]] =
    if (index == 0) Some(None)
    else ev.get(index - 1).map(Some(_))
}

// None, Some(0), Some(1), ...
case class IOption[A](ev: Infinite[A]) extends Infinite[Option[A]] {
  def apply(index: Z): Option[A] =
    if (index == 0) None
    else Some(ev(index - 1))
}
