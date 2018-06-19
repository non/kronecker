package kronecker

import shapeless._

trait CountableHListEv[H <: HList] {
  type FAux <: HList
  type IAux <: HList

  def infArity: Int
  def fbuild(index: Z): (Z, FAux)
  def ibuild(elem: List[Z]): IAux
  def combine(faux: FAux, iaux: IAux): H

  final def build(index: Z): H = {
    val (q, faux) = fbuild(index)
    val elem = Diagonal.atIndex(infArity, q)
    val iaux = ibuild(elem)
    combine(faux, iaux)
  }

  def cardinality: Card
}

class CountableHList[H <: HList](hm: CountableHListEv[H]) extends Countable[H] {
  def cardinality: Card = hm.cardinality
  def get(index: Z): Option[H] =
    if (cardinality.contains(index)) Some(hm.build(index)) else None
}

object CountableHListEv {

  implicit def hlistCons[A, H <: HList](implicit eva: Countable[A], evh: CountableHListEv[H]): CountableHListEv[A :: H] =
    eva.cardinality.value match {
      case Some(sz) => CountableHListEvFinite(eva, sz, evh)
      case None => CountableHListEvInfinite(eva, evh)
    }

  implicit object CountableHListEvHNil extends CountableHListEv[HNil] {
    type FAux = HNil
    type IAux = HNil
    def infArity: Int = 0
    def fbuild(index: Z): (Z, HNil) = (index, HNil)
    def ibuild(elem: List[Z]): HNil = if (elem.isEmpty) HNil else sys.error("!")
    def combine(faux: HNil, iaux: HNil): HNil = HNil
    def cardinality: Card = Card.one
  }

  case class CountableHListEvFinite[A, H <: HList](eva: Countable[A], sz: Z, evh: CountableHListEv[H]) extends CountableHListEv[A :: H] {
    type FAux = A :: evh.FAux
    type IAux = evh.IAux
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
    val cardinality: Card = evh.cardinality * eva.cardinality
  }

  case class CountableHListEvInfinite[A, H <: HList](eva: Countable[A], evh: CountableHListEv[H]) extends CountableHListEv[A :: H] {
    type FAux = evh.FAux
    type IAux = A :: evh.IAux
    def infArity: Int = evh.infArity + 1
    def fbuild(index: Z): (Z, evh.FAux) =
      evh.fbuild(index)
    def ibuild(elem: List[Z]): A :: evh.IAux =
      eva.get(elem.head).get :: evh.ibuild(elem.tail)
    def combine(faux: evh.FAux, iaux: A :: evh.IAux): A :: H =
      iaux.head :: evh.combine(faux, iaux.tail)
    def cardinality: Card = Card.infinite
  }
}
