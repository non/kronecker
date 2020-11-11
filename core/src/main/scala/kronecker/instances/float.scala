package kronecker
package instances

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
    reverseBits23(i)

  // the normal layout for 32-bit floats is:
  //
  //   seeeeeee emmmmmmm mmmmmmmm mmmmmmmm
  //
  // the encoding we use from index to float is:
  //
  //   mmmmmmmm mmmmmmmm mmmmmmme eeeeeees
  //
  // for the (m)antissa, (e)exponent, and (s)sign
  //
  // we reverse the mantissa (so most signficant bits are lowest), and
  // translate the exponent so that 127 comes first and then we
  // alternate up and down from there (i.e. 127, 128, 126, 129, ...)

  def get(index: Z): Option[Float] =
    if (cardinality.contains(index)) {
      val i = index.toInt
      val s = i & 1 // 1 bit
      val e = exponent((i >>> 1) & 0xff) // 8 bits
      val m = mantissa((i >>> 9) & 0x7fffff) // 23 bits
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
    val ee = if (e <= 127) (127 - e) * 2 else (e - 128) * 2 + 1
    val mm = reverseBits23(m)
    (Z(mm) << 9) | (Z(ee) << 1) | Z(s)
  }
}
