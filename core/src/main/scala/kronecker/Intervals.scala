package kronecker

import spire.math.{Interval, Searching}
import spire.math.extras.interval.IntervalSeq

object Intervals {

  case class AtOrAbove[A](x: A, b: Bounded[A]) extends Countable[A] {
    val cardinality: Card = b.upper match {
      case None => Card.infinite
      case Some(max) => Card(b.toZ(max) - b.toZ(x) + 1)
    }
    def get(index: Z): Option[A] = Some(b.offset(x, index))
  }

  case class AtOrBelow[A](y: A, b: Bounded[A]) extends Countable[A] {
    val cardinality: Card = b.lower match {
      case None => Card.infinite
      case Some(min) => Card(b.toZ(y) - b.toZ(min) + 1)
    }
    def get(index: Z): Option[A] = Some(b.offset(y, -index))
  }

  case class Within[A](first: A, last: A, b: Bounded[A]) extends Countable[A] {
    require(b.toZ(first) <= b.toZ(last))
    val cardinality: Card = Card(b.toZ(last) - b.toZ(first) + 1)
    def get(index: Z): Option[A] =
      if (cardinality.contains(index)) Some(b.offset(first, index)) else None
  }

  object CountableInterval {
    def apply[A](iv: Interval[A])(implicit b: Bounded[A]): Countable[A] =
      if (iv.isEmpty) {
        Countable.empty[A]
      } else {
        import spire.math.interval.{Closed, Open, Unbound, EmptyBound}
        val ofirst = iv.lowerBound match {
          case Closed(x) => Some(x)
          case Open(x) => Some(b.next(x))
          case Unbound() => None
          case EmptyBound() => sys.error("impossible!")
        }
        val olast = iv.upperBound match {
          case Closed(y) => Some(y)
          case Open(y) => Some(b.prev(y))
          case Unbound() => None
          case EmptyBound() => sys.error("impossible!")
        }
        (ofirst, olast) match {
          case (Some(x), Some(y)) => Within(x, y, b)
          case (Some(x), None) => AtOrAbove(x, b)
          case (None, Some(y)) => AtOrBelow(y, b)
          case (None, None) => b.countable
        }
      }
  }

  class CountableIntervalSeq[A](size: Z, ivs: Array[Countable[A]], offsets: Array[Z]) extends Countable[A] {
    val cardinality: Card =
      Card(size)

    def get(index: Z): Option[A] =
      if (cardinality.contains(index)) {
        val i = Searching.search(offsets, index)
        val j = if (i >= 0) i else -(i + 1) - 1
        val c = ivs(j)
        val k = index - offsets(j)
        c.get(k)
      } else {
        None
      }
  }

  object CountableIntervalSeq {
    def apply[A](seq: IntervalSeq[A])(implicit b: Bounded[A]): Countable[A] = {
      val cs = seq.intervalIterator
        .filter(_.nonEmpty)
        .map(CountableInterval(_))
        .toList
      Countable.oneOf(cs: _*)
    }
  }
}
