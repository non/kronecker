package kronecker

import scala.annotation.tailrec

/**
 * Provides codings for embedding maps in indices.
 *
 * The basic idea here is that in some cases we need to be able to
 * encode multiple logical indices into a single carrier index. Our
 * coding needs to be able to represent arbitrary lists of natural
 * numbers, without any "gaps" (lists of natural numbers that aren't
 * representable) or "duplicates" (lists of natural numbers with
 * multiple possible codings).
 *
 * CODING
 *
 * The strategy here is parameterized on a bit size k (where k > 1),
 * and the derived constant N = 2^k - 1.
 *
 * To code any logical number we first split it into digits base-N,
 * stored in k-bits. Each digit is then represented using k-bits, with
 * the adjustment that zero is coded with k 1-bits, instead of k
 * 0-bits. We do this because k 0-bits is used to mark the end of the
 * number.
 *
 * We also use carry rules to prevent duplicate representations
 * involving encoded zeros. If k 1-bits are seen, we set a carry flag
 * to true. If we reach the end of the number (k 0-bits) with the
 * carry flag set, we treat the final zero as a one. We will preserve
 * the carry flag as long as we see encoded zeros (k 1-bits) or ones
 * (just the one bit set).
 *
 * The digits are encoded in little-endian order, and the list
 * elements are also little-endian (i.e. the first logical index is
 * the smallest part of the carrier index).
 *
 * For lists with a maximum possible length, the final element is
 * treated as uncoded, to ensure we "consume" the remaining bits.
 *
 * CASE STUDY
 *
 * To support Countable[Map[K, V]] we need to be able to represent
 * each distinct map as a distinct index. We choose to do that by
 * representing any given map as a sequence of Option[V] values, one
 * value per possible key (using the enumeration from Countable[K] to
 * order the array). The maximum length of the sequence is the
 * cardinality of K.
 *
 * More specifically, every Map[Boolean, Z] value can be encoded as
 * two Option[Z] values (for the false key, then the true key),
 * which can then be represented as indices:
 *
 *    MAP VALUE                    | VALUE SEQUENCE     | INDICES
 *    Map()                        | None, None         | 0, 0
 *    Map(false -> 99)             | Some(99), None     | 198, 0
 *    Map(false -> 7)              | Some(7), None      | 15, 0
 *    Map(true -> -1)              | None, Some(-1)     | 0, 3
 *    Map(true -> -5, false -> 4)  | Some(4), Some(-5)  | 8, 11
 *    Map(false -> 33, true -> 44) | Some(33), Some(44) | 67, 89
 *
 * Our coding will then encode every index except the last, and
 * combine them. For example, given k=4, the above indices would be
 * coded as:
 *
 *    0, 0   | 0; [0]       | [0000].0000                |      0
 *    198, 0 | (13, 3); [0] | [0000].0000.1101.0011      |    211
 *    15, 0  | (1, 0); [0]  | [0000].0000.0000.1111      |     15
 *    0, 3   | 0; [3]       | [0011].0000                |     48
 *    8, 11  | 8; [11]      | [1011].0000.1000           |   2824
 *    67, 89 | (4, 7); [89] | [0101.1001].0000.0100.0111 | 364616
 *
 * (The final, uncoded index is surrounded in square brackets.)
 *
 * Codings are not needed when V is finite: in those cases we can just
 * use a single digit base-C, where C is the cardinality of V.
 */
object Coding {

  /**
   * Read an unbounded natural number in the given byte coding.
   *
   * The return value is `(indexAfterReading, valueRead)`.
   */
  def read(index: Z, mask: Int, k: Int): (Z, Z) = {
    @tailrec def loop(index0: Z, mult: Z, counter0: Z, carry0: Boolean): (Z, Z) =
      if (index0.isZero) {
        (index0, if (carry0) counter0 + mult else counter0)
      } else {
        val bits = (index0 & mask).toInt
        val index1 = index0 >> k
        if (bits == 0) {
          (index1, if (carry0) mult + counter0 else counter0)
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
