package kronecker

import scala.collection.mutable
import spire.math.{Interval, Searching}
import spire.math.extras.interval.IntervalSeq

object Intervals {

  class CountableInterval[A](size: Z, first: A)(implicit b: Bounded[A]) extends Countable[A] {
    val cardinality: Card =
      Card(size)
    def get(index: Z): Option[A] =
      if (cardinality.contains(index)) Some(b.offset(first, index)) else None
  }

  class CountableIntervalSeq[A](size: Z, ivs: Array[Countable[A]], offsets: Array[Z])(implicit b: Bounded[A]) extends Countable[A] {
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

  def resolve[A](iv: Interval[A])(implicit b: Bounded[A]): (A, A) = {
    import spire.math.interval._
    val first = iv.lowerBound match {
      case Closed(x) => x
      case Open(x) => b.next(x)
      case Unbound() => b.lower
      case EmptyBound() => sys.error("impossible!")
    }
    val last = iv.upperBound match {
      case Closed(y) => y
      case Open(y) => b.prev(y)
      case Unbound() => b.upper
      case EmptyBound() => sys.error("impossible!")
    }
    (first, last)
  }

  def countableForIntervalSeq[T](seq: IntervalSeq[T])(implicit b: Bounded[T]): Countable[T] = {
    var size: Z = Z.zero
    val ivbuf = mutable.ArrayBuffer.empty[Countable[T]]
    val ofbuf = mutable.ArrayBuffer.empty[Z]
    seq.intervalIterator.filter(_.nonEmpty).foreach { iv =>
      val (first, last) = resolve(iv)
      val sz = b.distance(first, last)
      ofbuf += size
      size += sz
      ivbuf += new CountableInterval(sz, first)
    }
    new CountableIntervalSeq(size, ivbuf.toArray, ofbuf.toArray)
  }
}
