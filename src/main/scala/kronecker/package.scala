import scala.annotation.tailrec

package object kronecker {
  type N = spire.math.Natural
  val N = spire.math.Natural

  type Z = spire.math.SafeLong
  val Z = spire.math.SafeLong

  type Q = spire.math.Rational
  val Q = spire.math.Rational

  type R = spire.math.Real
  val R = spire.math.Real

  def floor(x: R): Z = x.toRational.toSafeLong

  // 2^6553400 has 1972770 decimal digits
  val MaxExponent: Z = Z(6553400)

  def xpowOf(base: Z, exponent: Z): Card =
    if      (exponent.isZero)     Card.One
    else if (base.isZero)         Card.Zero
    else if (base.isOne)          Card.One
    else {
      // (2 ** k) approximates (base ** exponent)
      val k = exponent * base.bitLength
      if (k <= MaxExponent) Card.Finite(base.pow(exponent.toInt))
      else Card.Infinite
    }

  def powOf(base: Z, exponent: Z): Z =
    if      (exponent.isZero)     Z.one
    else if (base.isZero)         Z.zero
    else if (base.isOne)          Z.one
    else if (exponent.isValidInt) base.pow(exponent.toInt)
    else sys.error(s"$base^$exponent is too large")

  @tailrec def leftShift(n: Z, k: Z): Z =
    if (k.isValidInt) n << k.toInt
    else leftShift(n << Int.MaxValue, k - Int.MaxValue)
}
