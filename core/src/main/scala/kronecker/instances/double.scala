package kronecker
package instances

import java.lang.Double.{doubleToRawLongBits, longBitsToDouble}
import java.lang.Float.{floatToRawIntBits, intBitsToFloat}

object IndexableFloat extends Indexable[Float] {

  val cardinality: Card = Card(2) ** Card(32)

  private def sign(i: Int): (Int, Int) =
    (i & 1, i >>> 1)

  private def exponent(i: Int): (Int, Int) = {
    var e = 128
    if ((i & 1) == 0) {
      e -= ((i >>> 1) + 1)
    } else {
      e += (i >>> 1)
    }
    (e, i >>> 8)
  }

  private def mantissa(index: Int): (Int, Int) = {
    var i = index
    var m = 0
    var j = 0
    while (j < 23) {
      if ((i & 1) != 0) m |= (1 << (22 - j))
      i = i >>> 1
      j += 1
    }
    (m, i)
  }

  def get(index: Z): Option[Float] =
    if (cardinality.contains(index)) {
      val (s, i0) = sign(index.toInt)
      val (m, i1) = mantissa(i0)
      val (e, _) = exponent(i1)
      val bits = (s << 31) | (e << 23) | m
      Some(intBitsToFloat(bits))
    } else {
      None
    }

  def index(n: Float): Z = ???
}


object IndexableDouble extends Indexable[Double] {

  val cardinality: Card = Card(2) ** Card(64)

  private def sign(i: Long): (Long, Long) =
    (i & 1, i >>> 1)

  private def exponent(i: Long): (Long, Long) = {
    var e = 1024L
    if ((i & 1) == 0) {
      e -= ((i >>> 1) + 1)
    } else {
      e += (i >>> 1)
    }
    (e, i >>> 11)
  }

  private def mantissa(index: Long): (Long, Long) = {
    var i = index
    var m = 0L
    var j = 0
    while (j < 52) {
      if ((i & 1) != 0) m |= (1L << (51 - j))
      i = i >>> 1
      j += 1
    }
    (m, i)
  }

  def get(index: Z): Option[Double] =
    if (cardinality.contains(index)) {
      val (s, i0) = sign(index.toLong)
      val (m, i1) = mantissa(i0)
      val (e, _) = exponent(i1)
      val bits = (s << 63) | (e << 52) | m
      Some(longBitsToDouble(bits))
    } else {
      None
    }

  def index(n: Double): Z = ???
}
