package kronecker

import shapeless._

object CHList {
  def apply[H <: HList](implicit evh: HCountable[H]): Countable[H] =
    new Countable[H] {
      def cardinality: Card = evh.card
      def get(index: Z): Option[H] =
        if (evh.card.contains(index)) Some(evh.build(index)) else None
    }
}

sealed trait HCountable[H <: HList] {
  type FAux <: HList
  type IAux <: HList
  def card: Card
  def infArity: Int
  def fbuild(index: Z): (Z, FAux)
  def ibuild(elem: List[Z]): IAux
  def combine(faux: FAux, iaux: IAux): H
  final def build(index: Z): H = {
    val (q, faux) = fbuild(index)
    val elem = if (infArity == 0) Nil else Diagonal.atIndex(infArity, q)
    val iaux = ibuild(elem)
    combine(faux, iaux)
  }
}

object HCountable {

  implicit object CHNil extends HCountable[HNil] {
    type FAux = HNil
    type IAux = HNil
    val card: Card = Card.one
    def infArity: Int = 0
    def fbuild(index: Z): (Z, FAux) = (index, HNil)
    def ibuild(elem: List[Z]): IAux = if (elem.isEmpty) HNil else sys.error("!")
    def combine(faux: FAux, iaux: IAux): HNil = HNil
  }

  implicit def hcons[A, H <: HList](implicit eva: Countable[A], evh: HCountable[H]): HCountable[A :: H] =
    eva.cardinality.value match {
      case Some(sz) => Bounded(eva, sz, evh)
      case None => Unbounded(eva, evh)
    }

  case class Bounded[A, H <: HList](eva: Countable[A], sz: Z, evh: HCountable[H]) extends HCountable[A :: H] {
    type FAux = A :: evh.FAux
    type IAux = evh.IAux
    def card: Card = eva.cardinality * evh.card
    def infArity: Int = evh.infArity
    def fbuild(index: Z): (Z, A :: evh.FAux) = {
      val (q0, m0) = index /% sz
      val (q1, h) = evh.fbuild(q0)
      (q1, eva.get(m0).get :: h)
    }
    def ibuild(elem: List[Z]): evh.IAux =
      evh.ibuild(elem)
    def combine(faux: A :: evh.FAux, iaux: evh.IAux): A :: H =
      faux.head :: evh.combine(faux.tail, iaux)
  }

  case class Unbounded[A, H <: HList](eva: Countable[A], evh: HCountable[H]) extends HCountable[A :: H] {
    type FAux = evh.FAux
    type IAux = A :: evh.IAux
    def card: Card =
      Card.infinite
    def infArity: Int =
      evh.infArity + 1
    def fbuild(index: Z): (Z, evh.FAux) =
      evh.fbuild(index)
    def ibuild(elem: List[Z]): A :: evh.IAux =
      eva.get(elem.head).get :: evh.ibuild(elem.tail)
    def combine(faux: evh.FAux, iaux: A :: evh.IAux): A :: H =
      iaux.head :: evh.combine(faux, iaux.tail)
  }
}
