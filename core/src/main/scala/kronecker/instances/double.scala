package kronecker
package instances

import java.lang.Double.{doubleToRawLongBits, longBitsToDouble}
import java.lang.Float.{floatToRawIntBits, intBitsToFloat}

object IndexableFloat extends Indexable[Float] {

  // reverse lower 23 bits
  private def reverseBits23(n: Int): Int = {
    var input = n
    var output = 0
    var i = 0
    while (i < 23) {
      if ((input & 1) != 0) output |= (1 << (22 - i))
      input = input >>> 1
      i += 1
    }
    output
  }

  val cardinality: Card =
    Card(2) ** Card(32)

  private def exponent(i: Int): Int =
    if ((i & 1) == 0) 127 - (i / 2) else 128 + (i / 2)

  private def mantissa(i: Int): Int =
    reverseBits23(i & 0x3fffff)

  def get(index: Z): Option[Float] =
    if (cardinality.contains(index)) {
      val i = index.toInt
      val s = i & 1
      val m = mantissa(i >>> 1)
      val e = exponent(i >>> 24)
      val bits = (s << 31) | (e << 23) | m
      Some(intBitsToFloat(bits))
    } else {
      None
    }

  def index(x: Float): Z = {
    val bits = floatToRawIntBits(x)
    val s = (bits >>> 31) & 0x1 // 1 bit
    val e = (bits >>> 23) & 0xff // 8 bits
    val m = bits & 0x7fffff // 23 bits

    val ee = if (e <= 127) (127 - e) * 2 else (e - 128) * 2 - 1
    val mm = reverseBits23(m)
    (ee << 24) | (mm << 1) | s
  }
}


object IndexableDouble extends Indexable[Double] {

  // reverse lower 52 bits
  private def reverseBits52(n: Long): Long = {
    var input = n
    var output = 0L
    var i = 0
    while (i < 52) {
      if ((input & 1) != 0) output |= (1L << (51 - i))
      input = input >>> 1
      i += 1
    }
    output
  }

  val cardinality: Card =
    Card(2) ** Card(64)

  private def exponent(i: Long): Long =
    if ((i & 1) == 0L) 1023L - (i / 2) else 1024L + (i / 2)

  private def mantissa(index: Long): Long =
    reverseBits52(index & 0xfffffffffffffL)

  def get(index: Z): Option[Double] =
    if (cardinality.contains(index)) {
      val i = index.toLong
      val s = i & 1L
      val m = mantissa(i >>> 1)
      val e = exponent(i >>> 53)
      val bits = (s << 63) | (e << 52) | m
      Some(longBitsToDouble(bits))
    } else {
      None
    }

  def index(x: Double): Z = {
    val bits = doubleToRawLongBits(x)
    val s = (bits >>> 63) & 0x1L // 1 bit
    val e = (bits >>> 52) & 0x7ffL // 11 bits
    val m = bits & 0xfffffffffffffL // 52 bits

    val ee = if (e <= 1023) (1023 - e) * 2 else (e - 1023) * 2 - 1
    val mm = reverseBits52(m)
    (ee << 53) | (mm << 1) | s
  }
}
