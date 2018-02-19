package kronecker
package instances

import scala.annotation.tailrec

// Set(), Set(0), Set(1), Set(0, 1), Set(2), Set(0, 2), Set(1, 2),
// Set(0, 1, 2), Set(3), ...
case class FSet[A](ev: Finite[A]) extends Finite[Set[A]] {
  // NOTE: powOf() will crash for unsupportedly-large cardinalities
  val size: Z = powOf(Z(2), ev.size)

  def get(index: Z): Option[Set[A]] = {
    @tailrec def loop(rem: Z, index: Z, s0: Set[A]): Set[A] =
      if (rem.isZero || index >= ev.size) s0
      else {
        val s1 = if (rem.isOdd) s0 + ev.get(index).get else s0
        loop(rem >> 1, index + 1, s1)
      }
    if (index >= size) None
    else Some(loop(index, Z.zero, Set.empty))
  }
}

// Set(), Set(0), Set(1), Set(0, 1), Set(2), Set(0, 2), Set(1, 2),
// Set(0, 1, 2), Set(3), ...
case class ISet[A](ev: Infinite[A]) extends Infinite[Set[A]] {
  def apply(index: Z): Set[A] = {
    @tailrec def loop(rem: Z, index: Z, s0: Set[A]): Set[A] =
      if (rem.isZero) s0
      else if (rem.isOdd) loop(rem >> 1, index + 1, s0 + ev(index))
      else loop(rem >> 1, index + 1, s0)
    loop(index, Z.zero, Set.empty)
  }
}
