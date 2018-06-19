package kronecker
package instances

object CFunction {
  def apply[A, B](eva: Indexable[A], evb: Countable[B]): Countable[A => B] =
    (eva.cardinality.value, evb.cardinality.value) match {
      case (_, Some(szb)) =>
        new FFunction(eva, evb, szb)
      case (_, None) =>
        sys.error("!")
    }

  case class FFunction[A, B](eva: Indexable[A], evb: Countable[B], szb: Z) extends Countable[A => B] {

    val cardinality: Card = evb.cardinality ** eva.cardinality

    def get(index: Z): Option[A => B] =
      if (cardinality.contains(index)) {
        val f = Functional.function1(index, szb)
        Some(KFunction((eva, evb, index)) { (a: A) =>
          evb.get(f(eva.index(a))).get
        })
      } else {
        None
      }
  }
}
