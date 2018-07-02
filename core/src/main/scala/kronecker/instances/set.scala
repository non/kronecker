package kronecker
package instances

import scala.annotation.tailrec

object CSet {
  def apply[A](ev: Countable[A]): Countable[Set[A]] =
    ev.cardinality.value match {
      case Some(sz) => new CFSet(ev, sz)
      case None => new CISet(ev)
    }

  // Set(), Set(0), Set(1), Set(0, 1), Set(2), Set(0, 2), Set(1, 2),
  // Set(0, 1, 2), Set(3), ...
  class CFSet[A](ev: Countable[A], sz: Z) extends Countable[Set[A]] {
    val cardinality: Card = Card.two ** ev.cardinality
    def get(index: Z): Option[Set[A]] = {
      @tailrec def loop(rem: Z, index: Z, s0: Set[A]): Set[A] =
        if (rem.isZero || index >= sz) s0
        else {
          val s1 = if (rem.isOdd) s0 + ev.get(index).get else s0
          loop(rem >> 1, index + 1, s1)
        }
      if (cardinality.contains(index)) {
        Some(loop(index, Z.zero, Set.empty))
      } else {
        None
      }
    }
  }

  // Set(), Set(0), Set(1), Set(0, 1), Set(2), Set(0, 2), Set(1, 2),
  // Set(0, 1, 2), Set(3), ...
  class CISet[A](ev: Countable[A]) extends Countable[Set[A]] {
    val cardinality: Card = Card.two ** ev.cardinality
    def get(index: Z): Option[Set[A]] = {
      @tailrec def loop(rem: Z, index: Z, s0: Set[A]): Set[A] =
        if (rem.isZero) s0
        else if (rem.isOdd) loop(rem >> 1, index + 1, s0 + ev.get(index).get)
        else loop(rem >> 1, index + 1, s0)
      Some(loop(index, Z.zero, Set.empty))
    }
  }
}

object NSet {
  def apply[A](ev: Indexable[A]): Indexable[Set[A]] =
    ev.cardinality.value match {
      case Some(sz) => new NFSet(ev, sz)
      case None => new NISet(ev)
    }

  @tailrec def leftShift(n: Z, k: Z): Z =
    if (k.isValidInt) n << k.toInt
    else leftShift(n << Int.MaxValue, k - Int.MaxValue)

  class NFSet[A](ev: Indexable[A], sz: Z) extends CSet.CFSet(ev, sz) with Indexable[Set[A]] {
    def index(set: Set[A]): Z =
      set.foldLeft(Z.zero)((n, a) => n | NSet.leftShift(Z.one, ev.index(a)))
  }

  class NISet[A](ev: Indexable[A]) extends CSet.CISet(ev) with Indexable[Set[A]] {
    def index(set: Set[A]): Z =
      set.foldLeft(Z.zero)((n, a) => n | NSet.leftShift(Z.one, ev.index(a)))
  }
}
