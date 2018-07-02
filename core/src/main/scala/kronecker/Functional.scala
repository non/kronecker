package kronecker

import scala.annotation.tailrec

/**
 * Used to generate arbitrary function values.
 *
 * For example, consider:
 *
 *   sealed trait Quad
 *   case object A extends Quad
 *   case object B extends Quad
 *   case object C extends Quad
 *   case object D extends Quad
 *
 *   Quad => Boolean
 *
 * There are 4 Quad values, and 2 Boolean values, so there are 16
 * (2^4) functions of type Quad => Boolean. Each function can be
 * uniquely represented as a value 0-15 (i.e. in 4 bits):
 *
 *    0 (0000) -- returns false for all inputs
 *    1 (0001) -- returns true for A, false for all other inputs
 *    2 (0010) -- returns true for B, false for all other inputs
 *    3 (0011) -- returns true for A and B, false for all other inputs
 *    4 (0100) -- returns true for C, false for all other inputs
 *       ....
 *   15 (1111) -- returns true for all inputs
 *
 * (Not all types have a cardinality that is a power of two, but using
 * examples that do simplifies the above notation.)
 *
 * The idea here is that `index` specifies which function we want.
 * When actually want to evaluate the function for a given `input`, we
 * step through the index until we find the section that refers to the
 * given input value (0 would be first). Once there, we find the value
 * modulo `outWidth` and use that to determine which output value to
 * use.
 *
 * The `evaluate` function is general purpose and uses the division
 * and remainder operations. In `fastEvaluate` we special-case powers
 * of two to use bit-shifting and masking, which are significantly
 * faster for large numbers.
 */
object Functional {

  /**
   * Read an unbounded natural number in the given byte coding.
   *
   * The return value is `(valueRead, indexAfterReading)`.
   */
  def readCoding(index: Z, mask: Int, k: Int): (Z, Z) = {
    @tailrec def loop(index0: Z, mult: Z, counter0: Z, carry0: Boolean): (Z, Z) =
      if (index0.isZero) {
        (if (carry0) counter0 + mult else counter0, index0)
      } else {
        val bits = (index0 & mask).toInt
        val index1 = index0 >> k
        if (bits == 0) {
          (if (carry0) mult + counter0 else counter0, index1)
        } else {
          val n = if (bits == mask) 0 else bits
          val counter1 = if (n > 0) counter0 + (mult * n) else counter0
          val carry1 = n == 0 || (carry0 && n == 1)
          loop(index1, mult * mask, counter1, carry1)
        }
      }
    loop(index, Z.one, Z.zero, false)
  }
}
