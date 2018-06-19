package kronecker

import shapeless._

trait CountableCoproductEv[C <: Coproduct] {
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

class CountableCoproduct[C <: Coproduct](cm: CountableCoproductEv[C]) extends Countable[C] {
  def cardinality: Card = cm.cardinality
  def get(index: Z): Option[C] =
    if (cardinality.contains(index)) Some(cm.build(index)) else None
}

object CountableCoproductEv {

  implicit def cmixedCons[A, C <: Coproduct](implicit eva: Countable[A], evc: CountableCoproductEv[C]): CountableCoproductEv[A :+: C] =
    eva.cardinality.value match {
      case Some(sz) => new CountableCoproductEvFinite(eva, sz, evc)
      case None => new CountableCoproductEvInfinite(eva, evc)
    }

  implicit object CountableCoproductEvCNil extends CountableCoproductEv[CNil] {
    type FAux = CNil
    type IAux = CNil
    def cardinality: Card = Card.zero
    def finiteSize: Z = Z.zero
    def infArity: Int = 0
    def fget(index: Z): CNil = sys.error("!")
    def iget(index: Z, i: Int): CNil = sys.error("!")
  }

  class CountableCoproductEvFinite[A, C <: Coproduct](val eva: Countable[A], sz: Z, val evc: CountableCoproductEv[C]) extends CountableCoproductEv[A :+: C] {
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

  class CountableCoproductEvInfinite[A, C <: Coproduct](val eva: Countable[A], val evc: CountableCoproductEv[C]) extends CountableCoproductEv[A :+: C] {
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
