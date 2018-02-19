// package kronecker
// 
// /**
//  *
//  *
//  * Given (ec: Countable[A], ic: Indexable[A]) we require:
//  *
//  *   - 0 <= ic.index(a) < ec.cardinality, for a in A
//  *   - ec.get(ic.index(a)) = Some(a), for a in A
//  *   - ec.get(n).map(ic.index) = Some(n), for 0 <= n < ec.cardinality
//  */
// sealed trait Indexable[A] extends Countable[A] { self =>
//   def index(a: A): Z
// 
//   final def translate[B](f: B => A): Indexable[B] =
//     Indexable.instance(b => self.index(f(b)))
// }
// 
// object Indexable {
// 
//   def apply[A](implicit ev: Indexable[A]): Indexable[A] =
//     ev
// 
//   def instance[A](f: A => Z): Indexable[A] =
//     new Indexable[A] {
//       def index(a: A): Z = f(a)
//     }
// 
//   implicit val iz: Indexable[Z] =
//     instance { n =>
//       if (n.isZero) n
//       else if (n.signum > 0) n * 2 - 1
//       else -n * 2
//     }
// 
//   implicit val inothing: Indexable[Nothing] =
//     instance[Nothing](_ => sys.error("impossible"))
// 
//   implicit val iunit: Indexable[Unit] =
//     instance(_ => Z.zero)
// 
//   implicit val iboolean: Indexable[Boolean] =
//     instance(b => if (b) Z.one else Z.zero)
// 
//   implicit val ibyte: Indexable[Byte] =
//     instance(n => Z(n & 0xff))
// 
//   implicit val ishort: Indexable[Short] =
//     instance(n => Z(n & 0xffff))
// 
//   implicit val ichar: Indexable[Char] =
//     instance(c => Z(c))
// 
//   implicit val iint: Indexable[Int] =
//     instance(n => Z(n & 0xffffffffL))
// 
//   val BeyondLong = Z(1) << 64
// 
//   implicit val ilong: Indexable[Long] =
//     instance(n => if (n < 0) BeyondLong + n else Z(n))
// 
//   implicit val ibigint: Indexable[BigInt] =
//     iz.translate(n => Z(n))
// 
//   implicit def ioption[A](implicit ev: Indexable[A]): Indexable[Option[A]] =
//     instance {
//       case None => Z.zero
//       case Some(a) => ev.index(a) + 1
//     }
// }
