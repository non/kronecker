package kronecker

import shapeless._

// type class that witnesses to having a bunch of finite
// instances. for a type (A1 :+: A2 :+: ... An :+: CNil) we'll have
// Finite[A1], Finite[A2], ... Finite[An].
trait CFinite[H <: Coproduct] {
  def size: Z
  def get(index: Z): Option[H]
}

object CFinite{

  implicit object CFCNil extends CFinite[CNil] {
    def size: Z = Z.zero
    def get(index: Z): Option[CNil] = None
  }

  implicit def cfcons[A, C <: Coproduct](implicit eva: Countable.Finite[A], evc: CFinite[C]): CFinite[A :+: C] =
    new CFinite[A :+: C] {
      val size: Z = eva.size + evc.size
      def get(index: Z): Option[A :+: C] =
        if (index < eva.size) eva.get(index).map(Inl(_))
        else evc.get(index - eva.size).map(Inr(_))
    }
}
