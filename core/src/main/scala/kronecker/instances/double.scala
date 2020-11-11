package kronecker
package instances

import java.lang.Double.{doubleToRawLongBits, longBitsToDouble}

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

  private def mantissa(i: Long): Long =
    reverseBits52(i)

  // the normal layout for 64-bit floats is:
  //
  //   seeeeeee eeeemmmm mmmmmmmm mmmmmmmm mmmmmmmm mmmmmmmm mmmmmmmm mmmmmmmm
  //
  // the encoding we use from index to float is:
  //
  //   mmmmmmmm mmmmmmmm mmmmmmmm mmmmmmmm mmmmmmmm mmmmmmmm mmmmeeee eeeeeees
  //
  // for the (m)antissa, (e)exponent, and (s)sign
  //
  // we reverse the mantissa (so most signficant bits are lowest), and
  // translate the exponent so that 1023 comes first and then we
  // alternate up and down from there (i.e. 1023, 1024, 1022, 1025, ...)

  def get(index: Z): Option[Double] =
    if (cardinality.contains(index)) {
      val i = index.toLong
      val s = i & 1L // 1 bit
      val e = exponent((i >>> 1) & 0x7ffL) // 11 bits
      val m = mantissa((i >>> 12) & 0xfffffffffffffL) // 52 bits
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
    val ee = if (e <= 1023L) (1023L - e) * 2L else (e - 1024L) * 2L + 1L
    val mm = reverseBits52(m)
    (Z(mm) << 12) | (Z(ee) << 1) | Z(s)
  }
}
