package kronecker
package instances

object CFunction1 {
  def apply[A, B](eva: Indexable[A], evb: Countable[B]): Countable[A => B] = {
    lazy val zero: B = evb.get(0).get
    val cm = CMap(eva, evb.drop(1))
    cm.translate(m => TableFunction(m, zero))
  }
}
