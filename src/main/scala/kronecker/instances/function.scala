package kronecker
package instances

// this is a hack so we generate functions which can be compared with
// each other for equality.
private[kronecker] abstract class KFunction1[-A, +B](val id: AnyRef) extends Function1[A, B] {
  override def toString: String = s"KFunction1($id)"
  override def equals(that: Any): Boolean =
    that match {
      case f: AnyRef if this eq f => true
      case f: KFunction1[_, _] => id == f.id
      case _ => false
    }
  override def hashCode: Int =
    id.hashCode
}

object CFunction1 {
  def apply[A, B](eva: Indexable[A], evb: Countable[B]): Countable[A => B] =
    (eva.cardinality.value, evb.cardinality.value) match {
      case (_, Some(szb)) =>
        new FFunction1(eva, evb, szb)
      case (_, None) =>
        sys.error("TODO")
    }
}

case class FFunction1[A, B](eva: Indexable[A], evb: Countable[B], szb: Z) extends Countable[A => B] {

  val cardinality: Card = evb.cardinality ** eva.cardinality

  def get(index: Z): Option[A => B] =
    if (cardinality.contains(index)) {
      Some(new KFunction1[A, B]((index, eva, evb)) {
        val f = Functional.function1(index, szb)
        def apply(a: A): B = evb.get(f(eva.index(a))).get
      })
    } else None
}
