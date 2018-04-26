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

  /**
   * Construct an anonymous function Z => Z using evaluate or
   * fastEvaluate as appropriate.
   */
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


  /**
   * Given an `index` identifying a function, and an `input` to that
   * function, determine the function's output.
   *
   * We process `index` from least-significant bit to the
   * most-signficant (i.e. right-to-left). We use a similar encoding
   * to the one above for finite-ranged functions above, but use a
   * base-1 code to identify the output value.
   *
   * Suppose our index is represented as 110110101010011111011100111
   * in binary (i.e. 114638567).
   *
   * That value is interpreted as follows:
   *
   *   input:      9   8  7  6  5 4      3    2 1    0
   *   index:  [0]11-011-01-01-01-0-011111-0111-0-0111
   *   output:     2   2  1  1  1 0      5    3 0    3
   *
   * (The function returns 0 for all inputs > 9).
   *
   * Concretely this means that each output value is a (right-to-left)
   * series of zero or more 1-bits followed by a (possibly implicit)
   * 0-bit. The output value is the length of this series of 1-bits.
   *
   * Thus, the following interpretation of various indices:
   *
   *   index             function
   *
   *   0                 f(n) = 0 for all n >= 0
   *
   *   2^k - 1           f(0) = k
   *                     f(n) = 0 for all n > 0
   *
   *   (2^k - 1) << j    f(j) = k
   *                     f(n) = 0 for all n > 0 and n != j
   *
   *   ...10101010101    f(n) = 1 for all n >= 0
   *   (assuming index is an infinitely-large binary sequence)
   *
   * Keep in mind that the inputs and outputs of this function are
   * natural numbers (i.e. >= 0), even though we're implementing this
   * method using the type Z.
   */
  def infEvaluate1(index: Z, input: Z): Z = {
    @tailrec def loop(index0: Z, argIndex: Z, counter: Z): Z =
      if (index0.isZero) { if (argIndex == input) counter else Z.zero }
      else if (index0.isEven) loop(index0 >> 1, argIndex + 1, Z.zero)
      else loop(index0 >> 1, argIndex, counter + 1)
    loop(index, Z.zero, Z.zero)
  }


  /**
   * Given an `index` identifying a function, and an `input` to that
   * function, determine the function's output.
   *
   * This is similar to infEvaluate1, but is base-3 instead of base-1.
   *
   * We read right-to-left in 2-bit chunks. Each chunk is a terminator
   * (00) or else a ternary value of 0 (11), 1 (01), or 2 (10). All
   * values end with a (possibly implicit) terminator.
   *
   * One complication is the naive version of this encoding has
   * multiple representations of zero: 0, 11, 1111, and so on. To
   * account for this, when we see 11 we "carry" an implicit 01
   * following chunk, to be used if our sequence sees 00 next. We also
   * have to continue carrying if we see 01 next (since otherwise 11
   * and 0111 would both represent 3).
   *
   * Here are some examples of how this plays out:
   *
   *   input:         4      3    2      1  0
   *   index:  [00]1011-000111-0011-001001-00
   *   tern:        2 0  1 1 0  1 0    2 1  0
   *   output:        6     12    3      7  0
   */
  def infEvaluate3(index: Z, input: Z): Z =
    infEvaluate(index, input, 2)

  /**
   *              9   8  7   6 5 4 3   2 1  0
   *   index:  [0]f-01f-0a-033-0-0-0-02f-0-01
   *   base-15   10 110  a  33 0 0 0 020 0  1
   *   output:   15 240 10  48 0 0 0  30 0  1
   */
  def infEvaluate15(index: Z, input: Z): Z =
    infEvaluate(index, input, 4)


  /**
   * Given an `index` identifying a function, and an `input` to that
   * function, determine the function's output.
   *
   * This is similar to infEvaluate1, but instead of base-1 it is
   * base-mask (mask = 2^k - 1).
   *
   * We read right-to-left in k-bit chunks. Each chunk is a terminator
   * (all zeros), or a logical zero (all ones), or else its standard
   * value as a base-
   * (10). All values end with a (possibly implicit) terminator.
   *
   * One complication is the naive version of this encoding has
   * multiple representations of zero: 0, 11, 1111, and so on. To
   * account for this, when we see 11 we "carry" an implicit 01
   * following chunk, to be used if our sequence sees 00 next. We also
   * have to continue carrying if we see 01 next (since otherwise 11
   * and 0111 would both represent 3).
   */
  def infEvaluate(index: Z, input: Z, k: Int): Z = {
    require(k <= 31) //fixme
    val mask = (1 << k) - 1

    @tailrec def loop(index0: Z, argIndex: Z): Z =
      if (argIndex == input) readCoding(index0, mask, k)._2
      else if (index0.isZero) Z.zero
      else loop(readCoding(index0, mask, k)._1, argIndex + 1)

    loop(index, Z.zero)
  }

  /**
   * Read an unbounded natural number in the given byte coding.
   *
   * The return value is `(valueRead, indexAfterReading)`.
   */
  def readCoding(index: Z, mask: Int, k: Int): (Z, Z) = {
    @tailrec def loop(index0: Z, mult: Z, counter0: Z, carry0: Boolean): (Z, Z) = {

      if (index0.isZero) {
        (counter0, index0) // don't carry on the "final value"
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
    }
    loop(index, Z.one, Z.zero, false)
  }

  /**
   * Write the given value as coded number.
   *
   * - value: the value to be written
   * - mult: the multiplier (offset) to write this value at.
   *         should be divisible by mask.
   * - mask: the mask to use.
   * - k: the width of the shift and mask to use.
   *
   * Returns `(codedValue, newMult)`
   */
  def writeCoding(values: List[Z], k: Int): Z = {
    val mask = (1 << k) - 1
    val zmask = Z(mask)

    // returns (shift, output)
    def writeValue(value: Z, shift: Int, output: Z, isLast: Boolean): (Int, Z) = {
      //println(s"writeValue($value, $shift, $output, $isLast)")

      @tailrec def recur(value0: Z, shift0: Int, output0: Z, carry0: Boolean): (Int, Z) = {
        //println(s"recur($value0, $shift0, $output0, $carry0)")
        if (value0.isZero) {
          require(!carry0)
          (shift0, output0)
        } else {
          //val bits = (value0 & mask).toInt
          //val value1 = value0 >> k
          val bits = (value0 % mask).toInt
          val value1 = value0 / mask

          val (add1, carry1) =
            if (value1.isZero) {
              require(bits != 0)
              if (carry0 && bits == 1 && !isLast) (Z.zero, false)
              else (Z(bits), false)
            } else {
              if (bits == 0) (zmask, true)
              else if (bits == 1) (Z(1), carry0)
              else (Z(bits), false)
            }

          recur(value1, shift0 + k, output0 | (add1 << shift0), carry1)
        }
      }

      if (!value.isZero) recur(value, shift, output, false)
      else if (isLast) (shift + k, output | (mask << shift))
      else (shift + k, output)
    }

    @tailrec def loop(ns: List[Z], shift0: Int, output0: Z): Z =
      ns match {
        case Nil =>
          output0
        case n :: ns =>
          val isLast = ns.isEmpty
          if (n.isZero && !isLast) {
            loop(ns, shift0 + k, output0)
          } else {
            val (shift1, output1) = writeValue(n, shift0, output0, isLast)
            loop(ns, shift1 + k, output1)
          }
      }

    loop(values, 0, Z.zero)
  }
}
