package kronecker

import shapeless._

trait CMixed[C <: Coproduct] {
  type FAux <: Coproduct
  type IAux <: Coproduct

  def finiteSize: Z
  def infArity: Int
  def fget(index: Z): C
  def iget(index: Z, i: Int): C

  def cardinality: Card

  final def build(index: Z): C =
    if (index < finiteSize) fget(index)
    else if (infArity > 0) {
      val (q, m) = (index - finiteSize) /% infArity
      iget(q, m.toInt)
    } else {
      sys.error("!")
    }
}

class CMixedCountable[C <: Coproduct](cm: CMixed[C]) extends Countable[C] {
  def cardinality: Card = cm.cardinality
  def get(index: Z): Option[C] =
    if (cardinality.contains(index)) Some(cm.build(index)) else None
}

object CMixed {

  implicit def cmixedCons[A, C <: Coproduct](implicit eva: Countable[A], evc: CMixed[C]): CMixed[A :+: C] =
    eva.cardinality.value match {
      case Some(sz) => new CMixedFinite(eva, sz, evc)
      case None => new CMixedInfinite(eva, evc)
    }

  implicit object CMixedCNil extends CMixed[CNil] {
    type FAux = CNil
    type IAux = CNil
    def cardinality: Card = Card.zero
    def finiteSize: Z = Z.zero
    def infArity: Int = 0
    def fget(index: Z): CNil = sys.error("!")
    def iget(index: Z, i: Int): CNil = sys.error("!")
  }

  class CMixedFinite[A, C <: Coproduct](val eva: Countable[A], sz: Z, val evc: CMixed[C]) extends CMixed[A :+: C] {
    type FAux = A :+: evc.FAux
    type IAux = evc.IAux
    val finiteSize: Z = sz + evc.finiteSize
    val infArity: Int = evc.infArity
    val cardinality: Card = eva.cardinality + evc.cardinality
    def fget(index: Z): A :+: C =
      if (index < sz) Inl(eva.get(index).get)
      else Inr(evc.fget(index - sz))
    def iget(index: Z, i: Int): A :+: C =
      Inr(evc.iget(index, i))
  }

  class CMixedInfinite[A, C <: Coproduct](val eva: Countable[A], val evc: CMixed[C]) extends CMixed[A :+: C] {
    type FAux = evc.FAux
    type IAux = A :+: evc.IAux
    def cardinality: Card = Card.infinite
    val finiteSize: Z = evc.finiteSize
    val infArity: Int = 1 + evc.infArity
    def fget(index: Z): A :+: C =
      Inr(evc.fget(index))
    def iget(index: Z, i: Int): A :+: C =
      if (i == 0) Inl(eva.get(index).get) else Inr(evc.iget(index, i - 1))
  }
}
