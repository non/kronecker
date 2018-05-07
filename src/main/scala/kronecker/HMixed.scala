// package kronecker
// 
// import shapeless._
// 
// // type class that witnesses to having a bunch of infinite
// // instances. for a type (A1 :: A2 :: ... An :: HNil) we'll have
// // Finite[A1], Finite[A2], ... Finite[An].
// trait HMixed[H <: HList] {
//   type FAux <: HList
//   type IAux <: HList
//   def infArity: Int
//   def fbuild(index: Z): (Z, FAux)
//   def ibuild(elem: List[Z]): IAux
//   def combine(faux: FAux, iaux: IAux): H
// 
//   final def build(index: Z): H = {
//     val (q, faux) = fbuild(index)
//     val elem = Diagonal.atIndex(infArity, q)
//     val iaux = ibuild(elem)
//     combine(faux, iaux)
//   }
// }
// 
// object HMixed extends HMixedLowPri {
// 
//   implicit def fcons[A, H <: HList](implicit eva: Countable.Finite[A], evh: HMixed[H]): HMixed[A :: H] =
//     new HMixed[A :: H] {
//       type FAux = A :: evh.FAux
//       type IAux = evh.IAux
//       def infArity: Int = evh.infArity
//       def fbuild(index: Z): (Z, A :: evh.FAux) = {
//         val (q0, m0) = index /% eva.size
//         val (q1, h) = evh.fbuild(q0)
//         (q1, eva.get(m0).get :: h)
//       }
//       def ibuild(elem: List[Z]): evh.IAux =
//         evh.ibuild(elem)
//       def combine(faux: A :: evh.FAux, iaux: evh.IAux): A :: H =
//         faux.head :: evh.combine(faux.tail, iaux)
//     }
// 
//   implicit def icons[A, H <: HList](implicit eva: Countable.Infinite[A], evh: HMixed[H]): HMixed[A :: H] =
//     new HMixed[A :: H] {
//       type FAux = evh.FAux
//       type IAux = A :: evh.IAux
//       def infArity: Int =
//         evh.infArity + 1
//       def fbuild(index: Z): (Z, evh.FAux) =
//         evh.fbuild(index)
//       def ibuild(elem: List[Z]): A :: evh.IAux =
//         eva(elem.head) :: evh.ibuild(elem.tail)
//       def combine(faux: evh.FAux, iaux: A :: evh.IAux): A :: H =
//         iaux.head :: evh.combine(faux, iaux.tail)
//     }
// 
// }
// 
// abstract class HMixedLowPri {
// 
//   implicit def fcons2[A, H <: HList](implicit eva: Countable.Finite[A], evh: HInfinite[H]): HMixed[A :: H] =
//     new HMixed[A :: H] {
//       type FAux = A :: HNil
//       type IAux = H
//       def infArity: Int = evh.arity
//       def fbuild(index: Z): (Z, A :: HNil) = {
//         val (q0, m0) = index /% eva.size
//         (q0, eva.get(m0).get :: HNil)
//       }
//       def ibuild(elem: List[Z]): H =
//         evh.lookup(elem)
//       def combine(faux: A :: HNil, iaux: H): A :: H =
//         faux.head :: iaux
//     }
// 
//   implicit def icons2[A, H <: HList](implicit eva: Countable.Infinite[A], evh: HFinite[H]): HMixed[A :: H] =
//     new HMixed[A :: H] {
//       type FAux = H
//       type IAux = A :: HNil
//       def infArity: Int = 1
//       def fbuild(index: Z): (Z, H) = {
//         val (q, m) = index /% evh.size
//         (q, evh.get(m).get)
//       }
//       def ibuild(elem: List[Z]): A :: HNil =
//         eva(elem.head) :: HNil
//       def combine(faux: H, iaux: A :: HNil): A :: H =
//         iaux.head :: faux
//     }
// }
