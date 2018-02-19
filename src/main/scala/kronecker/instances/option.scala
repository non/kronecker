package kronecker
package instances

// None, Some(0), Some(1), ...
class CFOption[A](ev: Countable.Finite[A]) extends Countable.Finite[Option[A]] {
  val size: Z = ev.size + 1
  def get(index: Z): Option[Option[A]] =
    if (index == 0) Some(None)
    else ev.get(index - 1).map(Some(_))
}

// None, Some(0), Some(1), ...
class CIOption[A](ev: Countable.Infinite[A]) extends Countable.Infinite[Option[A]] {
  def apply(index: Z): Option[A] =
    if (index == 0) None
    else Some(ev(index - 1))
}

class NFOption[A](ev: Indexable.Finite[A]) extends CFOption(ev) with Indexable.Finite[Option[A]] {
  def index(o: Option[A]): Z =
    o match {
      case None => Z.zero
      case Some(a) => ev.index(a) + Z.one
    }
}

class NIOption[A](ev: Indexable.Infinite[A]) extends CIOption(ev) with Indexable.Infinite[Option[A]] {
  def index(o: Option[A]): Z =
    o match {
      case None => Z.zero
      case Some(a) => ev.index(a) + Z.one
    }
}
