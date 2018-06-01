package kronecker
package instances

// this is a hack so we generate functions which can be compared with
// each other for equality.
private[kronecker] abstract class KFunction[-A, +B](val id: AnyRef) extends Function1[A, B] {
  override def equals(that: Any): Boolean =
    that match {
      case f: AnyRef if this eq f => true
      case f: KFunction[_, _] => id == f.id
      case _ => false
    }
  override def hashCode: Int =
    id.hashCode
}

object KFunction {
  def apply[A, B](id: AnyRef)(f: A => B): KFunction[A, B] =
    new KFunction[A, B](id) {
      def apply(a: A): B = f(a)
    }
}

object CFunction {
  def apply[A, B](eva: Indexable[A], evb: Countable[B]): Countable[A => B] =
    (eva.cardinality.value, evb.cardinality.value) match {
      case (_, Some(szb)) =>
        new FFunction(eva, evb, szb)
      case (_, None) =>
        sys.error("!")
    }
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
