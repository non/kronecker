package kronecker
package refined

import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.{boolean => b}
import eu.timepit.refined.{numeric => n}

object implicits {

  private def unsafeMap[A, B](c: Countable[A])(f: A => B): Countable[B] =
    new Countable[B] {
      def cardinality = c.cardinality
      def get(index: Z): Option[B] = c.get(index).map(f)
    }

  implicit def countableForRefinedInt[X](implicit rx: Predicate[Int, X]): Countable[Refined[Int, X]] = {
    import spire.std.int._
    val iseq = rx.toIntervalSeq
    val c = Intervals.countableForIntervalSeq(iseq)
    unsafeMap(c) { x => Refined.unsafeApply[Int, X](x) }
  }

  implicit def countableForRefinedLong[X](implicit rx: Predicate[Long, X]): Countable[Refined[Long, X]] = {
    import spire.std.long._
    val iseq = rx.toIntervalSeq
    val c = Intervals.countableForIntervalSeq(iseq)
    unsafeMap(c) { x => Refined.unsafeApply[Long, X](x) }
  }

  def main(args: Array[String]): Unit = {
    val c = Countable[Refined[Int, b.And[n.Greater[W.`5`.T], n.Less[W.`20`.T]]]]

    println(c.cardinality)

    (0 until 20).foreach { i => println(c.get(i)) }
  }
}
