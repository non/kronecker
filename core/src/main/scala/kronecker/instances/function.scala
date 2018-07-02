package kronecker
package instances

object CFunction1 {
  def apply[A, B](eva: Indexable[A], evb: Countable[B]): Countable[A => B] = {
    val cm = CMap(eva, evb.drop(1))
    new Countable[A => B] {
      private lazy val zero: B =
        evb.get(0).get
      val cardinality: Card =
        cm.cardinality
      def get(index: Z): Option[A => B] =
        cm.get(index).map(m => TableFunction(m, zero))
    }
  }
}
