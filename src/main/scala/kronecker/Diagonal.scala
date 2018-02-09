package kronecker

import scala.annotation.tailrec
import spire.implicits._

/**
 * Utilities for doing diagonalization in N dimensions.
 *
 * The goal here is to be able to support diagonalizations for
 * arbitrary tuples, e.g. Tuple2, Tuple3, Tuple9, etc. The "dimension"
 * (or "dim") represents the arity of the tuple: dim=2 would
 * correspond to a Tuple2.
 *
 * We model each generated tuple as an "element" (a list of integers).
 * The individual integers in the element correspond to positions in
 * underlying streams of values, and the element corresponds to a
 * particular tuple.
 *
 * For example, if we have a type Ascii which corresponds to ASCII
 * strings, we might choose to represent the stream of all distinct
 * Ascii values as:
 *
 *    "a", "b", ... "z", "aa", "ab", ... "ba", ... "zz", "aaa", ...
 *
 * In this case, we could represent the stream of all distinct
 * Tuple2[Ascii, Ascii] values as:
 *
 *    ("a", "a"), ("b", "a"), ("a", "b"), ("c", "a"), ("b", "b"), ...
 *
 * This would correspond to the elements:
 *
 *    (0, 0), (1, 0), (0, 1), (2, 0), (1, 1), ...
 *
 * (where the integers index into the original stream of ASCII string
 * values above.)
 *
 * Here are some worked examples to get an idea of what is going on
 * with the indices. Each row of text here represents a "depth"
 * (depth=0 is the first row), and each logical column represents a
 * "position" at that depth. The "width" at a given depth is the
 * number of columns at that depth; for dim=1, width is always 1.
 *
 * dim=1
 * 0
 * 1
 * 2
 * 3
 *
 * dim=2
 * (0 0)
 * (1 0) (0 1)
 * (2 0) (1 1) (0 2)
 * (3 0) (2 1) (1 2) (0 3)
 * ...
 *
 * dim=3
 * (0 0 0)
 * (1 0 0) (0 1 0) (0 0 1)
 * (2 0 0) (1 1 0) (1 0 1) (0 2 0) (0 1 1) (0 0 2)
 * (3 0 0) (2 1 0) (2 0 1) (1 2 0) (1 1 1) (1 0 2) (0 3 0) (0 2 1) (0 1 2) (0 0 3)
 * ...
 *
 * and so on.
 *
 * The advantage of using Diagonal.atDepth is that it doesn't require
 * calculating all the preceeding elements in order to calculate an
 * element. This doesn't seem like a big deal until you consider that
 * at depth 30 a 20-tuple has over 47 trillion distinct elements.
 */
object Diagonal {

  type Elem = List[Z]

  /**
   * Determine how many elements of dimension `dim` exist at a given
   * tree `depth`.
   *
   * widthAtDepth(1, d) = 1
   * widthAtDepth(2, d) = d + 1
   * widthAtDepth(3, d) = ((d+1) * (d+2)) / 2
   * widthAtDepth(4, d) = ((d+1) * (d+2) * (d+3)) / 6
   * ...
   *
   * The sequences of these values are:
   *
   * dim=1: 1 1  1  1  1  1  1   1   1
   * dim=2: 1 2  3  4  5  6  7   8   9
   * dim=3: 1 3  6 10 15 21 28  36  45
   * dim=4: 1 4 10 16 31 52 80 116 161
   * ...
   *
   * Notice that the kth value at dimenion d is equal to the sum of
   * the (0 to kth) values at dimension (d-1).
   */
  def widthAtDepth(dim: Int, depth: Z): Z = {
    @tailrec def loop(i: Int, term: Z, num: Z, denom: Z): Z =
      if (i <= 1) num / denom
      else loop(i - 1, term + 1, (depth + term) * num, denom * term)

    if (dim == 1) Z.one else loop(dim, Z.one, Z.one, Z.one)
  }
  
  /**
   * Find a dimension `dim` element at the given `index`.
   *
   * This method decomposes the index into a depth and position and
   * then delegates the actual work to the `atDepth` method.
   */
  def atIndex(dim: Int, index: Z): Elem = {
    require(index >= 0)
    val (pos, depth) = decompose(dim, index)
    atDepth(dim, depth, pos)
  }

  /**
   * Find a dimension `dim` element at the given `depth` denoted by
   * position `pos`.
   *
   * Requirement: 0 <= pos < widthAtDepth(dim, depth)
   */
  def atDepth(dim: Int, depth: Z, pos: Z): Elem =
    dim match {
      case 1 =>
        depth :: Nil
      case d =>
        val (pos2, depth2) = decompose(dim - 1, pos)
        val elem = depth - depth2
        elem :: atDepth(dim - 1, depth2, pos2)
    }

  /**
   * Find the largest integer where the given property holds.
   *
   * This method does a binary search up the powers of 2 to find an
   * upper bound (an integer where the property fails), and then works
   * its way back through the remaining bits to figure out the largest
   * value where the property is still true.
   */
  def search(p: Z => Boolean): Z = {

    // search up to find the smallest power of 2 where p fails.
    @tailrec def ascend(i: Int): Int =
      if (p(Z.one << i)) ascend(i + 1) else i

    // we know that p(1 << ceil) is, but that p(1 << (ceil - 1)) is
    // true. so let's start adding bits back in (from highest to
    // lowest) to construct the largest value where p is still true
    @tailrec def descend(x: Z, i: Int): Z =
      if (i < 0) x else {
        val y = x | (Z.one << i)
        descend(if (p(y)) y else x, i - 1)
      }

    val ceil: Int = ascend(0)
    if (ceil == 0) Z.zero else descend(Z.zero, ceil - 1)
  }

  /**
   * Decompose an index into a position and depth.
   */
  def decompose(dim: Int, index: Z): (Z, Z) =
    if (dim == 1) {
      (Z.zero, index)
    } else {

      // it turns out that widthAtDepth(dim+1, d) equals:
      //   (0 to d).map(i => widthAtDepth(dim, i)).sum
      //
      // we want to calculate:
      //   (0 to (d-1)).map(i => widthAtDepth(dim, i)).sum
      //
      // so we use: widthAtDepth(dim+1, d-1)

      val k = search(n => widthAtDepth(dim + 1, n - 1) <= index)
      (index - widthAtDepth(dim + 1, k - 1), k)
    }
}
