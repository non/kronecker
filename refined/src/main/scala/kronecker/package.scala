package kronecker
package refined

import eu.timepit.refined.{boolean => b}
import eu.timepit.refined.{generic => g}
import eu.timepit.refined.{numeric => n}
import eu.timepit.refined.internal.{WitnessAs => W}

import spire.algebra.Order
import spire.math.{Interval, Searching}
import spire.math.extras.interval.IntervalSeq
import spire.implicits._

import scala.collection.mutable

object interval {

  class CountableInterval(size: Z, first: Long) extends Countable[Long] {
    val cardinality: Card =
      Card(size)
    def get(index: Z): Option[Long] =
      if (cardinality.contains(index)) Some(first + index.toLong) else None
  }

  class CountableIntervalSeq(size: Z, ivs: Array[Countable[Long]], sizes: Array[Z]) extends Countable[Long] {
    val cardinality: Card =
      Card(size)

    def get(index: Z): Option[Long] =
      if (cardinality.contains(index)) {
        val i = Searching.search(sizes, index)
        val j = if (i >= 0) i + 1 else -(i + 1)
        val c = ivs(j)
        val k = sizes(j) - c.cardinality.value.get
        c.get(k)
      } else {
        None
      }
  }

  def resolve(iv: Interval[Long]): (Long, Long) = {
    import spire.math.interval._
    val first = iv.lowerBound match {
      case Closed(x) => x
      case Open(x) => x + 1L
      case Unbound() => Long.MinValue
      case EmptyBound() => sys.error("impossible!")
    }
    val last = iv.upperBound match {
      case Closed(y) => y
      case Open(y) => y - 1L
      case Unbound() => Long.MaxValue
      case EmptyBound() => sys.error("impossible!")
    }
    (first, last)
  }

  def countableForIntervalSeq(seq: IntervalSeq[Long]): Countable[Long] = {
    var size: Z = Z.zero
    val ivbuf = mutable.ArrayBuffer.empty[Countable[Long]]
    val szbuf = mutable.ArrayBuffer.empty[Z]
    seq.intervalIterator.filter(_.nonEmpty).foreach { iv =>
      val (first, last) = resolve(iv)
      val sz = Z(last) - Z(first) + 1
      size += sz
      ivbuf += new CountableInterval(sz, first)
      szbuf += size
    }
    new CountableIntervalSeq(size, ivbuf.toArray, szbuf.toArray)
  }
}

sealed trait Refinement[T, P] {
  def toIntervalSeq(implicit ev: Order[T]): IntervalSeq[T] = {
    def recurse(r: Refinement[T, _]): IntervalSeq[T] =
      r match {
        case Refinement.Equal(wn) => IntervalSeq.point(wn.snd)
        case Refinement.Less(wn) => IntervalSeq.below(wn.snd)
        case Refinement.Greater(wn) => IntervalSeq.above(wn.snd)
        case Refinement.Not(r) => ~recurse(r)
        case Refinement.And(rp, rq) => recurse(rq) & recurse(rp)
        case Refinement.Or(rp, rq) => recurse(rq) | recurse(rp)
      }
    recurse(this)
  }
}

object Refinement {

  case class Equal[T, N](wn: W[N, T]) extends Refinement[T, g.Equal[N]]
  case class Less[T, N](wn: W[N, T]) extends Refinement[T, n.Less[N]]
  case class Greater[T, N](wn: W[N, T]) extends Refinement[T, n.Greater[N]]
  case class Not[T, P](r: Refinement[T, P]) extends Refinement[T, b.Not[P]]
  case class And[T, P, Q](rp: Refinement[T, P], rq: Refinement[T, Q]) extends Refinement[T, b.And[P, Q]]
  case class Or[T, P, Q](rp: Refinement[T, P], rq: Refinement[T, Q]) extends Refinement[T, b.Or[P, Q]]

  implicit def forLess[T, N](implicit wn: W[N, T]): Refinement[T, n.Less[N]] =
    Less(wn)

  implicit def forGreater[T, N](implicit wn: W[N, T]): Refinement[T, n.Greater[N]] =
    Greater(wn)

  implicit def forNot[T, P](implicit r: Refinement[T, P]): Refinement[T, b.Not[P]] =
    Not(r)

  implicit def forAnd[T, P, Q](implicit rp: Refinement[T, P], rq: Refinement[T, Q]): Refinement[T, b.And[P, Q]] =
    And(rp, rq)

  implicit def forOr[T, P, Q](implicit rp: Refinement[T, P], rq: Refinement[T, Q]): Refinement[T, b.Or[P, Q]] =
    Or(rp, rq)
}
