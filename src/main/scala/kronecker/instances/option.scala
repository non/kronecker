package kronecker
package instances

// None, Some(0), Some(1), ...
class COption[A](ev: Countable[A]) extends Countable[Option[A]] {
  def cardinality: Card = ev.cardinality + Card.one
  def get(index: Z): Option[Option[A]] =
    if (index == 0) Some(None) else ev.get(index - 1).map(Some(_))
}

class NOption[A](ev: Indexable[A]) extends COption(ev) with Indexable[Option[A]] {
  def index(o: Option[A]): Z =
    o match {
      case None => Z.zero
      case Some(a) => ev.index(a) + Z.one
    }
}
