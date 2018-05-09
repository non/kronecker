package kronecker

import shapeless._

object CCoproduct {
  def apply[C <: Coproduct](implicit evc: CCountable[C]): Countable[C] =
    new Countable[C] {
      def cardinality: Card = evc.card
      def get(index: Z): Option[C] =
        if (evc.card.contains(index)) Some(evc(index)) else None
    }
}

trait CCountable[C <: Coproduct] {
  type FAux <: Coproduct
  type IAux <: Coproduct

  def card: Card
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

object CCountable {

  implicit object CCNil extends CCountable[CNil] {
    type FAux = CNil
    type IAux = CNil

    def card: Card = Card.zero
    def finiteSize: Z = Z.zero
    def infArity: Int = 0
    def fget(index: Z): CNil = sys.error("!")
    def iapply(index: Z, i: Int): CNil = sys.error("!")
  }

  implicit def ccons[A, C <: Coproduct](implicit eva: Countable[A], evc: CCountable[C]): CCountable[A :+: C] =
    eva.cardinality.value match {
      case Some(sz) => Bounded(eva, sz, evc)
      case None => Unbounded(eva, evc)
    }

  case class Bounded[A, C <: Coproduct](eva: Countable[A], sz: Z, evc: CCountable[C]) extends CCountable[A :+: C] {
    type FAux = A :+: evc.FAux
    type IAux = evc.IAux
    val card: Card = eva.cardinality + evc.card
    val finiteSize: Z = sz + evc.finiteSize
    val infArity: Int = evc.infArity
    def fget(index: Z): A :+: C =
      if (index < sz) Inl(eva.get(index).get)
      else Inr(evc.fget(index - sz))
    def iapply(index: Z, i: Int): A :+: C =
      Inr(evc.iapply(index, i))
  }

  case class Unbounded[A, C <: Coproduct](eva: Countable[A], evc: CCountable[C]) extends CCountable[A :+: C] {
    type FAux = evc.FAux
    type IAux = A :+: evc.IAux
    val card = Card.infinite
    val finiteSize: Z = evc.finiteSize
    val infArity: Int = 1 + evc.infArity
    def fget(index: Z): A :+: C =
      Inr(evc.fget(index))
    def iapply(index: Z, i: Int): A :+: C =
      if (i == 0) Inl(eva.get(index).get) else Inr(evc.iapply(index, i - 1))
  }
}
