package kronecker
package instances

import shapeless._

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

  class ToCountable[H <: HList](evh: HCountable[H]) extends Countable[H] {
    def cardinality: Card =
      evh.card
    def get(index: Z): Option[H] =
      if (evh.card.contains(index)) Some(evh.build(index)) else None
  }

  sealed trait CHNil extends HCountable[HNil] {
    type FAux = HNil
    type IAux = HNil
    val card: Card = Card.one
    def infArity: Int = 0
    def fbuild(index: Z): (Z, FAux) = (index, HNil)
    def ibuild(elem: List[Z]): IAux = if (elem.isEmpty) HNil else sys.error("!")
    def combine(faux: FAux, iaux: IAux): HNil = HNil
  }

  class Bounded[A, H <: HList](eva: Countable[A], sz: Z, val evh: HCountable[H]) extends HCountable[A :: H] {
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

  class Unbounded[A, H <: HList](eva: Countable[A], val evh: HCountable[H]) extends HCountable[A :: H] {
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

  implicit object CHNil extends CHNil

  implicit def hcons[A, H <: HList](implicit eva: Countable[A], evh: HCountable[H]): HCountable[A :: H] =
    eva.cardinality.value match {
      case Some(sz) => new Bounded(eva, sz, evh)
      case None => new Unbounded(eva, evh)
    }
}

sealed trait HIndexable[H <: HList] extends HCountable[H] {
  def split(h: H): (FAux, IAux)
  def funbuild(faux: FAux): (Z, Z)
  def iunbuild(iaux: IAux): List[Z]

  final def unbuild(h: H): Z = {
    val (faux, iaux) = split(h)
    val (i, n) = funbuild(faux)
    val j = Diagonal.fromElem(iunbuild(iaux))
    i + (j * n)
  }
}

object HIndexable {

  class ToIndexable[H <: HList](evh: HIndexable[H]) extends HCountable.ToCountable(evh) with Indexable[H] {
    def index(h: H): Z = evh.unbuild(h)
  }

  sealed trait IHNil extends HCountable.CHNil with HIndexable[HNil] {
    def split(h: HNil): (FAux, IAux) = (HNil, HNil)
    def funbuild(faux: FAux): (Z, Z) = (Z.zero, Z.one)
    def iunbuild(iaux: IAux): List[Z] = Nil
  }

  class Bounded[A, H <: HList](eva: Indexable[A], sz: Z, override val evh: HIndexable[H])
      extends HCountable.Bounded[A, H](eva, sz, evh) with HIndexable[A :: H] {
    def split(elem: A :: H): (FAux, IAux) = {
      val (fx, ix) = evh.split(elem.tail)
      (elem.head :: fx, ix)
    }
    def funbuild(faux: FAux): (Z, Z) = {
      val (i, n) = evh.funbuild(faux.tail)
      val j = eva.index(faux.head) + (sz * i)
      (j, n * sz)
    }
    def iunbuild(iaux: IAux): List[Z] =
      evh.iunbuild(iaux)
  }

  class Unbounded[A, H <: HList](eva: Indexable[A], override val evh: HIndexable[H])
      extends HCountable.Unbounded[A, H](eva, evh) with HIndexable[A :: H] {
    def split(elem: A :: H): (FAux, IAux) = {
      val (fx, ix) = evh.split(elem.tail)
      (fx, elem.head :: ix)
    }
    def funbuild(faux: FAux): (Z, Z) =
      evh.funbuild(faux)
    def iunbuild(iaux: IAux): List[Z] =
      eva.index(iaux.head) :: evh.iunbuild(iaux.tail)
  }

  implicit object IHNil extends IHNil

  implicit def hcons[A, H <: HList](implicit eva: Indexable[A], evh: HIndexable[H]): HIndexable[A :: H] =
    eva.cardinality.value match {
      case Some(sz) => new Bounded(eva, sz, evh)
      case None => new Unbounded(eva, evh)
    }
}
