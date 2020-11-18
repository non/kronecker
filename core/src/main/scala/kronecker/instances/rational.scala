package kronecker
package instances

import spire.math.Rational

/*
 */

object IndexableRational extends Indexable[Rational] {

  def cardinality: Card =
    Card.Infinite


  def get(index: Z): Option[Rational] = {

    // given a 1-indexed position in the sequence, return the
    // corresponding rational.
    //
    // sequence is: 1, 1/2, 2, 1/3, 3/2, 2/3, 3, 1/4, ...
    //
    // inverse of index/find.
    def find(k: Z): Rational = {

      // most significant byte is first
      val bytes: Array[Byte] = k.toBigInt.bigInteger.toByteArray

      // find the starting 1 digit and skip it
      var i = if (bytes(0) == 0) 1 else 0
      var mask = 128
      while (mask > 0 && (bytes(i) & mask) == 0) mask >>= 1
      mask >>= 1

      // now do the algorithm, i.e. keep branching smaller (on 0) or
      // bigger (on 1) until we run out of binary digits.
      var a = Z.one
      var b = Z.one
      while (i < bytes.length) {
        val byte = bytes(i)
        while (mask > 0) {
          if ((byte & mask) == 0) {
            b = a + b
          } else {
            a = a + b
          }
          mask >>= 1
        }
        mask = 128
        i += 1
      }
      Rational(a, b)
    }

    // 0 is a sentinel, then odd indices are positive rationals, and
    // even indices are negative rationals.
    if (index.isZero) Some(Rational.zero)
    else if ((index & 1) == 1) Some(find((index + 1) / 2))
    else Some(-find(index / 2))
  }

  def index(r: Rational): Z = {

    // inverse of get/find, only handles positive rationals, returns
    // 1-indexed position in the sequence.
    def find(n: Z, d: Z): Z = {
      var a = n
      var b = d
      var result = Z.zero
      var mask = Z.one
      while (!(a.isOne && b.isOne)) {
        if (a > b) {
          result += mask
          a -= b
        } else {
          b -= a
        }
        mask = mask << 1
      }
      result + mask
    }

    if (r.isZero) Z.zero
    else if (r.signum > 0) find(r.numerator, r.denominator) * 2 - 1
    else find(-r.numerator, r.denominator) * 2
  }
}
