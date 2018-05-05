package kronecker
package instances

import Countable.{Finite, Infinite}

// this is a hack so we generate functions which can be compared with
// each other for equality.
private[kronecker] abstract class KFunction[-A, +B](val id: AnyRef) extends Function1[A, B] {
  override def equals(that: Any): Boolean =
    that match {
      case f: AnyRef if this eq f => true
      case f: KFunction[_, _] => id == f.id
      case _ => false
    }
  override def hashCode: Int =
    id.hashCode
}

case class FFFunction[A, B](ca: Finite[A], ia: Indexable[A], cb: Finite[B]) extends Finite[A => B] {

  // NOTE: powOf() will crash for unsupportedly-large cardinalities
  val size: Z = powOf(cb.size, ca.size)

  def get(index: Z): Option[A => B] =
    if (index >= size) None
    else Some(new KFunction[A, B]((ca, ia, cb, index)) {
      val f = Functional.function1(index, cb.size)
      def apply(a: A): B = cb.get(f(ia.index(a))).get
    })
}

case class IFFunction[A, B](ca: Infinite[A], ia: Indexable[A], cb: Finite[B]) extends Infinite[A => B] {
  def apply(index: Z): A => B =
    new KFunction[A, B]((ca, ia, cb, index)) {
      val f = Functional.function1(index, cb.size)
      def apply(a: A): B = cb.get(f(ia.index(a))).get
    }
}

// this is currently busted. for example consider Unit => String.
//
// in this case we really should just interpret the index as the
// particular string constant to return, but instead we'll chooose
// some argIndex that is way too big for Indexable[Unit].

// case class XIFunction[A, B](ia: Indexable[A], cb: Infinite[B]) extends Infinite[A => B] {
//   def apply(index: Z): A => B =
//     (a: A) => cb(Functional.infEvaluate1(index, ia.index(a)))
// }
