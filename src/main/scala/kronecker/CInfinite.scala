package kronecker

import shapeless._

// type class that witnesses to having a bunch of infinite
// instances. for a type (A1 :+: A2 :+: ... An :+: CNil) we'll have
// Infinite[A1], Infinite[A2], ... Infinite[An].
trait CInfinite[C <: Coproduct] {
  def arity: Int
  def apply0(index: Z, i: Int): C

  final def apply(index: Z): C = {
    val (q, m) = index /% arity
    apply0(q, m.toInt)
  }
}

object CInfinite{

  // implicit object CFCNil extends CInfinite[CNil] {
  //   def arity: Int = 0
  //   def apply0(index: Z, i: Int): CNil = sys.error("unreachable")
  // }

  implicit def cflast[A](implicit eva: Countable.Infinite[A]): CInfinite[A :+: CNil] =
    new CInfinite[A :+: CNil] {
      val arity: Int = 1
      def apply0(index: Z, i: Int): A :+: CNil =
        if (i == 0) Inl(eva(index)) else sys.error("!")
    }

  implicit def cfcons[A, C <: Coproduct](implicit eva: Countable.Infinite[A], evc: CInfinite[C]): CInfinite[A :+: C] =
    new CInfinite[A :+: C] {
      val arity: Int = 1 + evc.arity
      def apply0(index: Z, i: Int): A :+: C =
        if (i == 0) Inl(eva(index)) else Inr(evc.apply0(index, i - 1))
    }
}
