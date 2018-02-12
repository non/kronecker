package kronecker

import shapeless._

trait CMixed[C <: Coproduct] {
  type FAux <: Coproduct
  type IAux <: Coproduct

  def finiteSize: Z
  def infArity: Int
  def fget(index: Z): C
  def iapply(index: Z, i: Int): C

  final def apply(index: Z): C =
    if (index < finiteSize) fget(index)
    else {
      val (q, m) = (index - finiteSize) /% infArity
      iapply(q, m.toInt)
    }
}

object CMixed {
  implicit object CCNil extends CMixed[CNil] {
    type FAux = CNil
    type IAux = CNil
    def finiteSize: Z = Z.zero
    def infArity: Int = 0
    def fget(index: Z): CNil = sys.error("unreachable")
    def iapply(index: Z, i: Int): CNil = sys.error("unreachable")
  }

  implicit def fcons[A, C <: Coproduct](implicit eva: Finite[A], evc: CMixed[C]): CMixed[A :+: C] =
    new CMixed[A :+: C] {
      type FAux = A :+: evc.FAux
      type IAux = evc.IAux
      val finiteSize: Z = eva.size + evc.finiteSize
      val infArity: Int = evc.infArity
      def fget(index: Z): A :+: C =
        if (index < eva.size) Inl(eva.get(index).get)
        else Inr(evc.fget(index - eva.size))
      def iapply(index: Z, i: Int): A :+: C =
        Inr(evc.iapply(index, i))
    }

  implicit def icons[A, C <: Coproduct](implicit eva: Infinite[A], evc: CMixed[C]): CMixed[A :+: C] =
    new CMixed[A :+: C] {
      type FAux = evc.FAux
      type IAux = A :+: evc.IAux
      val finiteSize: Z = evc.finiteSize
      val infArity: Int = 1 + evc.infArity
      def fget(index: Z): A :+: C =
        Inr(evc.fget(index))
      def iapply(index: Z, i: Int): A :+: C =
        if (i == 0) Inl(eva(index)) else Inr(evc.iapply(index, i - 1))
    }
}
