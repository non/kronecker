package kronecker

/**
 * Functions that are tagged for equality.
 *
 * Two functions with the same `id` should behave identically. We
 * consider these two functions to be equal.
 *
 * This is primarily used to assist law-checking for type classes
 * dealing with functions.
 */
sealed abstract class KFunction[-A, +B](val id: AnyRef) extends Function1[A, B] {
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
