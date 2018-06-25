package kronecker

import spire.algebra.Eq
import scala.util.control.NonFatal
import spire.algebra.{Bool, PartialOrder}
import spire.implicits._

abstract class Predicate {

  def isTrue: Boolean
  def message: String

  final lazy val result: Predicate.Result =
    try {
      if (isTrue) Predicate.Result.True
      else Predicate.Result.False(message)
    } catch { case NonFatal(e) =>
      Predicate.Result.Error(e)
    }
}

object Predicate {

  sealed abstract class Result
  sealed abstract class Counterexample extends Result

  object Result {
    case object True extends Result
    case object Skip extends Result
    case class False(s: String) extends Counterexample
    case class Error(t: Throwable) extends Counterexample
  }

  // basic predicates

  case class IsTrue(b: Boolean) extends Predicate {
    def isTrue: Boolean = b
    def message: String = s"($b.isTrue)"
  }

  case class Satisfies[A](lhs: A, rhs: A => Boolean) extends Predicate {
    def isTrue: Boolean = rhs(lhs)
    def message: String = s"($lhs satisfies $rhs)"
  }

  // combinators

  case class Not(p: Predicate) extends Predicate {
    def isTrue: Boolean = !p.isTrue
    def message: String = s"(not ${p.message})"
  }

  case class And(p1: Predicate, p2: Predicate) extends Predicate {
    def isTrue: Boolean = p1.isTrue && p2.isTrue
    def message: String = s"(${p1.message} and ${p2.message})"
  }

  case class Or(p1: Predicate, p2: Predicate) extends Predicate {
    def isTrue: Boolean = p1.isTrue || p2.isTrue
    def message: String = s"(${p1.message} or ${p2.message})"
  }

  case class Xor(p1: Predicate, p2: Predicate) extends Predicate {
    def isTrue: Boolean = p1.isTrue ^ p2.isTrue
    def message: String = s"(${p1.message} xor ${p2.message})"
  }

  case class Nor(p1: Predicate, p2: Predicate) extends Predicate {
    def isTrue: Boolean = !(p1.isTrue || p2.isTrue)
    def message: String = s"(${p1.message} nor ${p2.message})"
  }

  case class Nand(p1: Predicate, p2: Predicate) extends Predicate {
    def isTrue: Boolean = !(p1.isTrue && p2.isTrue)
    def message: String = s"(${p1.message} nand ${p2.message})"
  }

  case class Implies(p1: Predicate, p2: Predicate) extends Predicate {
    def isTrue: Boolean = !p1.isTrue || p2.isTrue
    def message: String = s"(${p1.message} implies ${p2.message})"
  }

  // equality

  case class EqualTo[A](lhs: A, rhs: A, e: Eq[A]) extends Predicate {
    def isTrue: Boolean = e.eqv(lhs, rhs)
    def message: String = s"($lhs equalTo $rhs)"
  }

  case class NotEqualTo[A](lhs: A, rhs: A, e: Eq[A]) extends Predicate {
    def isTrue: Boolean = e.neqv(lhs, rhs)
    def message: String = s"($lhs notEqualTo $rhs)"
  }

  case class RefersTo[A <: AnyRef](lhs: A, rhs: A) extends Predicate {
    def isTrue: Boolean = lhs eq rhs
    def message: String = s"($lhs refersTo $rhs)"
  }

  case class IsNull[A <: AnyRef](lhs: A) extends Predicate {
    def isTrue: Boolean = lhs == null
    def message: String = s"($lhs = null)"
  }

  case class NonNull[A <: AnyRef](lhs: A) extends Predicate {
    def isTrue: Boolean = lhs != null
    def message: String = s"($lhs != null)"
  }

  // comparisons

  sealed abstract class Comparisons[A](val symbol: String) extends Predicate {
    def lhs: A
    def rhs: A
    def partialOrder: PartialOrder[A]
    def comparison(n: Double): Boolean
    final def isTrue: Boolean = comparison(partialOrder.partialCompare(lhs, rhs))
    final def message: String = s"($lhs $symbol $rhs)"
  }

  case class Lt[A](lhs: A, rhs: A, partialOrder: PartialOrder[A]) extends Comparisons[A]("<") {
    def comparison(n: Double): Boolean = n < 0.0
  }

  case class LtEq[A](lhs: A, rhs: A, partialOrder: PartialOrder[A]) extends Comparisons[A]("<=") {
    def comparison(n: Double): Boolean = n <= 0.0
  }

  case class Gt[A](lhs: A, rhs: A, partialOrder: PartialOrder[A]) extends Comparisons[A](">") {
    def comparison(n: Double): Boolean = n > 0.0
  }

  case class GtEq[A](lhs: A, rhs: A, partialOrder: PartialOrder[A]) extends Comparisons[A](">=") {
    def comparison(n: Double): Boolean = n >= 0.0
  }

  // collections

  case class IsEmpty[A](lhs: Iterable[A]) extends Predicate {
    def isTrue: Boolean = lhs.isEmpty
    def message: String = s"($lhs.isEmpty)"
  }

  case class NonEmpty[A](lhs: Iterable[A]) extends Predicate {
    def isTrue: Boolean = lhs.nonEmpty
    def message: String = s"($lhs.nonEmpty)"
  }

  case class Contains[A](lhs: Iterable[A], rhs: A) extends Predicate {
    def isTrue: Boolean = lhs.toSet(rhs)
    def message: String = s"($lhs contains $rhs)"
  }

  case class ContainsAllOf[A](lhs: Iterable[A], rhs: Iterable[A]) extends Predicate {
    def isTrue: Boolean = rhs.toSet.subsetOf(lhs.toSet)
    def message: String = s"($lhs containsAllOf $rhs)"
  }

  case class ContainsAnyOf[A](lhs: Iterable[A], rhs: Iterable[A]) extends Predicate {
    def isTrue: Boolean = (lhs.toSet intersect rhs.toSet).nonEmpty
    def message: String = s"($lhs containsAnyOf $rhs)"
  }

  case class ContainsNoneOf[A](lhs: Iterable[A], rhs: Iterable[A]) extends Predicate {
    def isTrue: Boolean = (lhs.toSet intersect rhs.toSet).isEmpty
    def message: String = s"($lhs containsNoneOf $rhs)"
  }

  case class Forall[A](lhs: Iterable[A], rhs: A => Boolean) extends Predicate {
    def isTrue: Boolean = lhs.forall(rhs)
    def message: String = s"($lhs.forall($rhs))"
  }

  case class Exists[A](lhs: Iterable[A], rhs: A => Boolean) extends Predicate {
    def isTrue: Boolean = lhs.exists(rhs)
    def message: String = s"($lhs.exists($rhs))"
  }

  case class Atom[A](lhs: A) {
    def isTrue(implicit ev: A =:= Boolean): Predicate = IsTrue(ev(lhs))
    def satisfies(p: A => Boolean): Predicate = Satisfies(lhs, p)
    def equalTo(rhs: A)(implicit ev: Eq[A]): Predicate = EqualTo(lhs, rhs, ev)
    def notEqualTo(rhs: A)(implicit ev: Eq[A]): Predicate = NotEqualTo(lhs, rhs, ev)
    def refersTo(rhs: A)(implicit ev: A <:< AnyRef): Predicate = RefersTo(ev(lhs), ev(rhs))
    def isNull(implicit ev: A <:< AnyRef): Predicate = IsNull(ev(lhs))
    def nonNull(implicit ev: A <:< AnyRef): Predicate = NonNull(ev(lhs))

    def <(rhs: A)(implicit ev: PartialOrder[A]): Predicate = Lt(lhs, rhs, ev)
    def <=(rhs: A)(implicit ev: PartialOrder[A]): Predicate = LtEq(lhs, rhs, ev)
    def >(rhs: A)(implicit ev: PartialOrder[A]): Predicate = Gt(lhs, rhs, ev)
    def >=(rhs: A)(implicit ev: PartialOrder[A]): Predicate = GtEq(lhs, rhs, ev)

    def isEmpty[B](implicit ev: A =:= Iterable[B]): Predicate = IsEmpty(ev(lhs))
    def nonEmpty[B](implicit ev: A =:= Iterable[B]): Predicate = NonEmpty(ev(lhs))
    def contains[B](rhs: B)(implicit ev: A =:= Iterable[B]): Predicate = Contains(ev(lhs), rhs)
    def containsAllOf[B](rhs: Iterable[B])(implicit ev: A =:= Iterable[B]): Predicate = ContainsAllOf(ev(lhs), rhs)
    def containsAnyOf[B](rhs: Iterable[B])(implicit ev: A =:= Iterable[B]): Predicate = ContainsAnyOf(ev(lhs), rhs)
    def containsNoneOf[B](rhs: Iterable[B])(implicit ev: A =:= Iterable[B]): Predicate = ContainsNoneOf(ev(lhs), rhs)
    def forall[B](rhs: B => Boolean)(implicit ev: A =:= Iterable[B]): Predicate = Forall(ev(lhs), rhs)
    def exists[B](rhs: B => Boolean)(implicit ev: A =:= Iterable[B]): Predicate = Exists(ev(lhs), rhs)
  }
}
