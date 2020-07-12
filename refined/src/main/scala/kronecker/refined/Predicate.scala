package kronecker
package refined

import eu.timepit.refined.{boolean => b}
import eu.timepit.refined.{generic => g}
import eu.timepit.refined.{numeric => n}

import spire.algebra.Order
import spire.math.extras.interval.IntervalSeq

/**
 *
 */
sealed trait Predicate[T, P] {
  def toIntervalSeq(implicit ev: Order[T]): IntervalSeq[T] = {
    def recurse(r: Predicate[T, _]): IntervalSeq[T] =
      r match {
        case Predicate.Equal(wn) => IntervalSeq.point(wn.snd)
        case Predicate.Less(wn) => IntervalSeq.below(wn.snd)
        case Predicate.Greater(wn) => IntervalSeq.above(wn.snd)
        case Predicate.Not(r) => ~recurse(r)
        case Predicate.And(rp, rq) => recurse(rq) & recurse(rp)
        case Predicate.Or(rp, rq) => recurse(rq) | recurse(rp)
      }
    recurse(this)
  }
}

object Predicate {

  import eu.timepit.refined.internal.{WitnessAs => W}

  case class Equal[T, N](wn: W[N, T]) extends Predicate[T, g.Equal[N]]
  case class Less[T, N](wn: W[N, T]) extends Predicate[T, n.Less[N]]
  case class Greater[T, N](wn: W[N, T]) extends Predicate[T, n.Greater[N]]
  case class Not[T, P](r: Predicate[T, P]) extends Predicate[T, b.Not[P]]
  case class And[T, P, Q](rp: Predicate[T, P], rq: Predicate[T, Q]) extends Predicate[T, b.And[P, Q]]
  case class Or[T, P, Q](rp: Predicate[T, P], rq: Predicate[T, Q]) extends Predicate[T, b.Or[P, Q]]

  implicit def forLess[T, N](implicit wn: W[N, T]): Predicate[T, n.Less[N]] =
    Less(wn)

  implicit def forGreater[T, N](implicit wn: W[N, T]): Predicate[T, n.Greater[N]] =
    Greater(wn)

  implicit def forNot[T, P](implicit r: Predicate[T, P]): Predicate[T, b.Not[P]] =
    Not(r)

  implicit def forAnd[T, P, Q](implicit rp: Predicate[T, P], rq: Predicate[T, Q]): Predicate[T, b.And[P, Q]] =
    And(rp, rq)

  implicit def forOr[T, P, Q](implicit rp: Predicate[T, P], rq: Predicate[T, Q]): Predicate[T, b.Or[P, Q]] =
    Or(rp, rq)
}
