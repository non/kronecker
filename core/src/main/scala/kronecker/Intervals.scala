package kronecker

import spire.algebra.Order
import spire.math.{Interval, Searching}
import spire.math.extras.interval.IntervalSeq
import spire.implicits._

import scala.collection.mutable

object Intervals {

  case class AtOrAbove(x: Z) extends Countable[Z] {
    val cardinality: Card = Card.infinite
    def get(index: Z): Option[Z] = Some(x + index)
  }

  case class AtOrBelow(y: Z) extends Countable[Z] {
    val cardinality: Card = Card.infinite
    def get(index: Z): Option[Z] = Some(y - index)
  }

  case class Within(first: Z, last: Z) extends Countable[Z] {
    require(first <= last)
    val cardinality: Card = Card(last - first + 1)
    def get(index: Z): Option[Z] =
      if (cardinality.contains(index)) Some(first + index) else None
  }

  object CountableInterval {
    def apply(iv: Interval[Z]): Countable[Z] = {

      def close(iv: Interval[Z]): (Option[Z], Option[Z])= {
        import spire.math.interval._
        val first = iv.lowerBound match {
          case Closed(x) => Some(x)
          case Open(x) => Some(x + 1)
          case Unbound() => None
          case EmptyBound() => sys.error("impossible!")
        }
        val last = iv.upperBound match {
          case Closed(y) => Some(y)
          case Open(y) => Some(y - 1)
          case Unbound() => None
          case EmptyBound() => sys.error("impossible!")
        }
        (first, last)
      }

      if (iv.isEmpty) Countable.empty[Z]
      else close(iv) match {
        case (Some(x), Some(y)) => Within(x, y)
        case (Some(x), None) => AtOrAbove(x)
        case (None, Some(y)) => AtOrBelow(y)
        case (None, None) => Countable[Z]
      }
    }
  }

  object CountableIntervalSeq {
    def apply(seq: IntervalSeq[Z]): Countable[Z] = {
      val cs = seq.intervalIterator
        .filter(_.nonEmpty)
        .map(CountableInterval(_))
        .toList
      Countable.oneOf(cs: _*)
    }
  }
}
