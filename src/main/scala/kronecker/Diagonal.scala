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
   * widthAtDepth(0, d) = 1
   * widthAtDepth(1, d) = d + 1
   * widthAtDepth(2, d) = ((d+1) * (d+2)) / 2
   * widthAtDepth(3, d) = ((d+1) * (d+2) * (d+3)) / 6
   * ...
   */
  def widthAtDepth(dim: Int, depth: Z): Z = {
    @tailrec def loop(i: Int, term: Z, num: Z, denom: Z): Z =
      if (i <= 0) num / denom
      else loop(i - 1, term + 1, (depth + term) * num, denom * term)
    loop(dim, Z.one, Z.one, Z.one)
  }

  /**
   * Find a dimension `dim` element at the given `index`.
   *
   * This method decomposes the index into a depth and position and
   * then delegates the actual work to the `atDepth` method.
   */
  def atIndex(dim: Int, index: Z): Elem = {
    val (pos, depth) = decompose(dim, index)
    atDepth(dim, depth, pos)
  }

  /**
   * Find a dimension `dim` element at the given `depth` denoted by
   * position `pos`.
   *
   * Requirement: 0 <= pos < widthAtDepth(dim, depth)
   */
  def atDepth(dim: Int, depth: Z, pos: Z): Elem = {
    require(dim >= 1, s"($dim >= 1) was false")
    require(depth >= 0, s"($depth >= 0) was false")
    val w = widthAtDepth(dim, depth)
    require(0 <= pos && pos < w, s"(0 <= $pos && $pos < $w) was false")
    atDepth0(dim, depth, pos)
  }

  /**
   * Same as atDepth but without the input validation.
   */
  def atDepth0(dim: Int, depth: Z, pos: Z): Elem =
    dim match {
      case 1 =>
        depth :: Nil
      case d =>
        val (pos2, depth2) = decompose(dim - 1, pos)
        val elem = depth - depth2
        elem :: atDepth(dim - 1, depth2, pos2)
    }

  /**
   * Decompose an index into a depth and position.
   */
  def decompose(dim: Int, index: Z): (Z, Z) = {
    @tailrec def loop(curr: Z, dp: Z): (Z, Z) = {
      val w = widthAtDepth(dim - 1, dp)
      val next = curr - w
      if (next >= 0) loop(next, dp + 1) else (curr, dp)
    }
    loop(index, Z.zero)
  }
}
