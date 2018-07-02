package kronecker

import shapeless._

sealed trait CCountable[C <: Coproduct] {
  type FAux <: Coproduct
  type IAux <: Coproduct

  def card: Card
  def finiteSize: Z
  def infArity: Int
  def fbuild(index: Z): C
  def ibuild(index: Z, i: Int): C

  final def build(index: Z): C =
    if (index < finiteSize) fbuild(index)
    else {
      val (q, m) = (index - finiteSize) /% infArity
      ibuild(q, m.toInt)
    }
}

object CCountable {

  sealed class ToCountable[C <: Coproduct](evc: CCountable[C]) extends Countable[C] {
    def cardinality: Card =
      evc.card
    def get(index: Z): Option[C] =
      if (evc.card.contains(index)) Some(evc.build(index)) else None
  }

  sealed trait CCNil extends CCountable[CNil] {
    type FAux = CNil
    type IAux = CNil

    def card: Card = Card.zero
    def finiteSize: Z = Z.zero
    def infArity: Int = 0
    def fbuild(index: Z): CNil = sys.error("impossible")
    def ibuild(index: Z, i: Int): CNil = sys.error("impossible")
  }

  class Bounded[A, C <: Coproduct](eva: Countable[A], sz: Z, val evc: CCountable[C]) extends CCountable[A :+: C] {
    type FAux = A :+: evc.FAux
    type IAux = evc.IAux
    val card: Card = eva.cardinality + evc.card
    val finiteSize: Z = sz + evc.finiteSize
    val infArity: Int = evc.infArity
    def fbuild(index: Z): A :+: C =
      if (index < sz) Inl(eva.get(index).get)
      else Inr(evc.fbuild(index - sz))
    def ibuild(index: Z, i: Int): A :+: C =
      Inr(evc.ibuild(index, i))
  }

  class Unbounded[A, C <: Coproduct](eva: Countable[A], val evc: CCountable[C]) extends CCountable[A :+: C] {
    type FAux = evc.FAux
    type IAux = A :+: evc.IAux
    val card = Card.infinite
    val finiteSize: Z = evc.finiteSize
    val infArity: Int = 1 + evc.infArity
    def fbuild(index: Z): A :+: C =
      Inr(evc.fbuild(index))
    def ibuild(index: Z, i: Int): A :+: C =
      if (i == 0) Inl(eva.get(index).get) else Inr(evc.ibuild(index, i - 1))
  }

  implicit object CCNil extends CCNil

  implicit def ccons[A, C <: Coproduct](implicit eva: Countable[A], evc: CCountable[C]): CCountable[A :+: C] =
    eva.cardinality.value match {
      case Some(sz) => new Bounded(eva, sz, evc)
      case None => new Unbounded(eva, evc)
    }

}

sealed trait CIndexable[C <: Coproduct] extends CCountable[C] {
  def split(c: C): Either[FAux, IAux]
  def funbuild(faux: FAux): Z
  def iunbuild(iaux: IAux): (Z, Int)

  final def unbuild(c: C): Z =
    split(c) match {
      case Left(faux) =>
        funbuild(faux)
      case Right(iaux) =>
        val (index, i) = iunbuild(iaux)
        (index * infArity + i) + finiteSize
    }
}

object CIndexable {

  final class ToIndexable[C <: Coproduct](evc: CIndexable[C])
      extends CCountable.ToCountable(evc) with Indexable[C] {
    def index(c: C): Z = evc.unbuild(c)
  }

  final class Bounded[A, C <: Coproduct](eva: Indexable[A], sz: Z, override val evc: CIndexable[C])
      extends CCountable.Bounded[A, C](eva, sz, evc) with CIndexable[A :+: C] {
    def split(elem: A :+: C): Either[FAux, IAux] =
      elem match {
        case Inl(a) =>
          Left(Inl(a))
        case Inr(c) =>
          evc.split(c) match {
            case Left(faux) => Left(Inr(faux))
            case Right(iaux) => Right(iaux)
          }
      }
    def funbuild(faux: FAux): Z =
      faux match {
        case Inl(a) => eva.index(a)
        case Inr(fs) => evc.funbuild(fs) + sz
      }
    def iunbuild(iaux: IAux): (Z, Int) =
      evc.iunbuild(iaux)
  }

  final class Unbounded[A, C <: Coproduct](eva: Indexable[A], override val evc: CIndexable[C])
      extends CCountable.Unbounded[A, C](eva, evc) with CIndexable[A :+: C] {
    def split(elem: A :+: C): Either[FAux, IAux] =
      elem match {
        case Inl(a) =>
          Right(Inl(a))
        case Inr(c) =>
          evc.split(c) match {
            case Left(faux) => Left(faux)
            case Right(iaux) => Right(Inr(iaux))
          }
      }
    def funbuild(faux: FAux): Z =
      evc.funbuild(faux)
    def iunbuild(iaux: IAux): (Z, Int) =
      iaux match {
        case Inl(a) =>
          (eva.index(a), 0)
        case Inr(fs) =>
          val (index, i) = evc.iunbuild(fs)
          (index, i + 1)
      }
  }

  implicit object ICNil extends CCountable.CCNil with CIndexable[CNil] {
    def split(elem: CNil): Either[FAux, IAux] = sys.error("impossible")
    def funbuild(faux: FAux): Z = sys.error("impossible")
    def iunbuild(iaux: IAux): (Z, Int) = sys.error("impossible")
  }

  implicit def icons[A, C <: Coproduct](implicit eva: Indexable[A], evc: CIndexable[C]): CIndexable[A :+: C] =
    eva.cardinality.value match {
      case Some(sz) => new Bounded(eva, sz, evc)
      case None => new Unbounded(eva, evc)
    }
}
