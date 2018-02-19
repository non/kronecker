package kronecker
package instances

import Countable.{Finite, Infinite}

case class FFFunction[A, B](ca: Finite[A], ia: Indexable[A], cb: Finite[B]) extends Finite[A => B] {

  // NOTE: powOf() will crash for unsupportedly-large cardinalities
  val size: Z = powOf(cb.size, ca.size)

  def get(index: Z): Option[A => B] =
    if (index >= size) None
    else {
      val f = Functional.function1(index, cb.size)
      Some((a: A) => cb.get(f(ia.index(a))).get)
    }
}

case class IFFunction[A, B](ca: Infinite[A], ia: Indexable[A], cb: Finite[B]) extends Infinite[A => B] {
  def apply(index: Z): A => B = {
    val f = Functional.function1(index, cb.size)
    (a: A) => cb.get(f(ia.index(a))).get
  }
}

case class XIFunction[A, B](ia: Indexable[A], cb: Infinite[B]) extends Infinite[A => B] {
  def apply(index: Z): A => B =
  (a: A) => cb(Functional.infEvaluate1(index, ia.index(a)))
}
