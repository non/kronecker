// package kronecker
// 
// import shapeless._
// 
// // type class that witnesses to having a bunch of infinite
// // instances. for a type (A1 :: A2 :: ... An :: HNil) we'll have
// // Finite[A1], Finite[A2], ... Finite[An].
// trait HFinite[H <: HList] {
//   def size: Z
//   def get(index: Z): Option[H]
// }
// 
// object HFinite{
// 
//   implicit object HINil extends HFinite[HNil] {
//     def size: Z = Z.one
//     def get(index: Z): Option[HNil] =
//       if (index == 0) Some(HNil) else None
//   }
// 
//   implicit def hicons[A, H <: HList](implicit eva: Countable.Finite[A], evh: HFinite[H]): HFinite[A :: H] =
//     new HFinite[A :: H] {
//       val size: Z = eva.size * evh.size
//       def get(index: Z): Option[A :: H] = {
//         val (q, r) = index /% eva.size
//         for {
//           h <- eva.get(r)
//           t <- evh.get(q)
//         } yield h :: t
//       }
//     }
// }
