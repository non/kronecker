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

  def function1(index: Z, outWidth: Z): Function1[Z, Z] =
    if (outWidth.toBigInt.bitCount == 1) {
      // outWidth is a power of two
      val mask = outWidth - 1
      val shift = outWidth.bitLength - 1
      (input: Z) => fastEvaluate(index, input, mask, shift)
    } else {
      // outWidth is not a power of two
      (input: Z) => evaluate(index, input, outWidth)
    }

  def fastEvaluate(index: Z, input: Z, outMask: Z, outShift: Int): Z = {
    @tailrec def loop(index0: Z, argIndex: Z): Z =
      if (index0.isZero) Z.zero
      else if (argIndex >= input) index0 & outMask
      else loop(index0 >> outShift, argIndex + 1)
    loop(index, Z.zero)
  }

  def evaluate(index: Z, input: Z, outWidth: Z): Z = {
    @tailrec def loop(index0: Z, argIndex: Z): Z =
      if (index0.isZero) Z.zero
      else if (argIndex >= input) index0 % outWidth
      else loop(index0 / outWidth, argIndex + 1)
    loop(index, Z.zero)
  }

  def infEvaluate(index: Z, input: Z): Z = {
    @tailrec def loop(index0: Z, argIndex: Z, counter: Z): Z =
      if (argIndex > input) counter
      else if (index0.isZero) { if (argIndex == input) counter else Z.zero }
      else if (index0.isEven) loop(index0 >> 1, argIndex + 1, Z.zero)
      else loop(index0 >> 1, argIndex, counter + 1)
    loop(index, Z.zero, Z.zero)
  }
}
