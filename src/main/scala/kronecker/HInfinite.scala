package kronecker

import shapeless._

// type class that witnesses to having a bunch of infinite
// instances. for a type (A1 :: A2 :: ... An :: HNil) we'll have
// Infinite[A1], Infinite[A2], ... Infinite[An].
trait HInfinite[H <: HList] {
  def arity: Int
  def lookup(elem: List[Z]): H
}

object HInfinite{

  implicit object HINil extends HInfinite[HNil] {
    def arity: Int = 0
    def lookup(elem: List[Z]): HNil = {
      require(elem.isEmpty, elem.toString)
      HNil
    }
  }

  implicit def hicons[A, H <: HList](implicit eva: Infinite[A], evh: HInfinite[H]): HInfinite[A :: H] =
    new HInfinite[A :: H] {
      def arity: Int = evh.arity + 1
      def lookup(elem: List[Z]): A :: H =
        eva(elem.head) :: evh.lookup(elem.tail)
    }
}
