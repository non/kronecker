package kronecker

import spire.algebra.Bool
import spire.implicits._

abstract class Predicate[-A] { lhs =>

  def apply(a: A): Boolean

  def contramap[Z](f: Z => A): Predicate[Z] =
    Predicate(z => lhs(f(z)))
}

object Predicate {

  def apply[A](p: A => Boolean): Predicate[A] =
    new Predicate[A] {
      def apply(a: A): Boolean = p(a)
    }

  implicit def booleanPredicate[A]: Bool[Predicate[A]] =
    new Bool[Predicate[A]] {
      def zero: Predicate[A] = False
      def one: Predicate[A] = True
      def and(x: Predicate[A], y: Predicate[A]): Predicate[A] =
        Predicate(a => x(a) && y(a))
      def or(x: Predicate[A], y: Predicate[A]): Predicate[A] =
        Predicate(a => x(a) || y(a))
      def complement(x: Predicate[A]): Predicate[A] =
        Predicate(a => !x(a))
    }

  val True: Predicate[Any] =
    Predicate(_ => true)

  val False: Predicate[Any] =
    Predicate(_ => false)
}
