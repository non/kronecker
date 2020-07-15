package kronecker
package refined

import eu.timepit.refined.api.Refined
import spire.algebra.Order

object implicits {

  /**
   * Provides Countable instances for supported Refinement types.
   *
   * Currently the only predicates supported are those which slice
   * interval types into intervals, such as:
   *
   *     import eu.timepit.refined._
   *     import eu.timepit.refined.api.Refined
   *     import eu.timepit.refined.{boolean => b}
   *     import eu.timepit.refined.{numeric => n}
   *     Countable[Refined[Int, b.And[n.Greater[W.`5`.T], n.Less[W.`20`.T]]]]
   *
   * Adding more supported number types requires (at least) instances
   * for Bounded[A] and Order[A]. In particular, floating point types
   * are not likely to be easy to support using the current strategy.
   */
  implicit def countableForRefined[A, R](
    implicit p: Predicate[A, R],
    b: Bounded[A],
    o: Order[A]
  ): Countable[Refined[A, R]] =
    new Countable[Refined[A, R]] {
      val c = Intervals.CountableIntervalSeq(p.toIntervalSeq)
      def cardinality =
        c.cardinality
      def get(index: Z): Option[Refined[A, R]] =
        c.get(index).map(Refined.unsafeApply)
    }
}
