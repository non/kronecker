package kronecker

/**
 * Permutation represents a rearrangement of ordered values.
 *
 * For example, given the list [a, b, c], and p = Permutation.empty,
 * here are the following permutations:
 *
 *  - p                       [a, b, c]
 *  - p.swap(a, b)            [b, a, c]
 *  - p.swap(a, c)            [c, b, a]
 *  - p.swap(a, c).swap(a, b) [b, c, a]
 *
 * Internally, permutation has the invariant that its remappings must
 * form a cycle; for any x0 one of the following must be true:
 *
 *  - p(x0) = x0
 *  - p(x0) = x1, p(x1) = x0
 *  - p(x0) = x1, p(x1) = x2, p(x2) = x0
 *  - and so on...
 *
 * The permutation preserves the elements in the underlying list, but
 * just changes their positions. Using a permutation to reorder a
 * list, it should be impossible to "lose" any values.
 */
case class Permutation[A] private (m: Map[A, A]) {

  /**
   * Apply the permutation to a particular apply.
   *
   * Apply is a bijection.
   */
  def apply(a0: A): A =
    m.get(a0) match {
      case Some(a1) => a1
      case None => a0
    }

  /**
   * Compose two permutations.
   *
   * This is equivalent to treating permutations as functions and
   * combining their apply methods with `compose`.
   *
   * x compose y = y andThen x
   */
  def compose(p: Permutation[A]): Permutation[A] =
    p andThen this

  /**
   * Compose two permutations.
   *
   * This is equivalent to treating permutations as functions and
   * combining their apply methods with `andThen`.
   *
   * x compose y = y andThen x
   */
  def andThen(p: Permutation[A]): Permutation[A] =
    Permutation(p.m.foldLeft(m) { case (m0, (k, v)) => up(m0, k, this(v)) })

  /**
   * Reverse a permutation.
   *
   * Reversing a permutation is equivalent to inverting its apply
   * method.
   */
  def reverse: Permutation[A] =
    Permutation(m.map { case (a0, a1) => (a1, a0) })

  /**
   * Product a new permutation while preserving the invariant that all
   * mappings must form a loop.
   */
  def swap(x0: A, y0: A): Permutation[A] =
    if (x0 == y0) this
    else Permutation(up(up(m, x0, this(y0)), y0, this(x0)))

  private def up(m: Map[A, A], k: A, v: A): Map[A, A] =
    if (k == v) m - k
    else m.updated(k, v)
}

object Permutation {

  /**
   * Return the identity permutation.
   */
  def identity[A]: Permutation[A] =
    Permutation(Map.empty)

  /**
   * Build a permutation that swaps exactly two values.
   *
   * The permutation is otherwise equivalent to the identity
   * function.
   */
  def apply[A](a0: A, a1: A): Permutation[A] =
    identity[A].swap(a0, a1)
}
