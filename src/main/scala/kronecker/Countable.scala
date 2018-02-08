package kronecker

import scala.annotation.tailrec

import java.lang.Double.longBitsToDouble
import java.lang.Float.intBitsToFloat
import shapeless._

trait Countable[A] {
  def cardinality: Card
  def get(index: Z): Option[A]
}

trait Finite[A] extends Countable[A] {
  def size: Z

  final def cardinality: Card = Card.Finite(size)
}

trait Infinite[A] extends Countable[A] {
  def apply(index: Z): A

  final def cardinality: Card = Card.Infinite
  final def get(index: Z): Some[A] = Some(apply(index))
}

object Countable {

  def apply[A](implicit ev: Countable[A]): Countable[A] = ev

  // .
  implicit val cnothing: Finite[Nothing] =
    Integers(Z.zero, _ => sys.error("impossible"))

  // ().
  implicit val cunit: Finite[Unit] =
    Integers(Z.one, _ => ())

  // false, true.
  implicit val cboolean: Finite[Boolean] =
    Integers(Z.two, _ == Z.one)

  // TODO: the signed streams could be in a nicer order; right now the
  // negatives only come after you exhaust the positives.

  // 0, 1, 2, ..., 127, -128, -127, ..., -1.
  implicit val cbyte: Finite[Byte] =
    Integers(Z.one << 8, _.toByte)

  // 0, 1, 2, ..., 65535, -65536, -65535, ..., -1.
  implicit val cshort: Finite[Short] =
    Integers(Z.one << 16, _.toShort)

  // 0, 1, 2, ... -1.
  implicit val cint: Finite[Int] =
    Integers(Z.one << 32, _.toInt)

  // 0, 1, 2, ... -1.
  implicit val clong: Finite[Long] =
    Integers(Z.one << 64, _.toLong)

  // TODO: the floating point streams could be in a MUCH nicer order;
  // the current order is pretty much nonsense.

  implicit val cfloat: Finite[Float] =
    Integers(Z.one << 32, n => intBitsToFloat(n.toInt))

  implicit val cdouble: Finite[Double] =
    Integers(Z.one << 64, n => longBitsToDouble(n.toLong))

  // 0, 1, -1, 2, -2, ...
  implicit val cz: Infinite[Z] =
    new Infinite[Z] {
      def apply(index: Z): Z = {
        val (quot, mod) = (index + 1) /% Z.two
        val sign = Z.one - (mod * Z.two)
        sign * quot
      }
    }

  // this only works if the A type is finite. A types that are
  // infinite require a different strategy than the lexicographic
  // order (since you can never "finish" the length=1 lists you have
  // to interleave larger ones).
  implicit def lexicographicList[A](implicit ev: Finite[A]): Infinite[List[A]] =
    new Infinite[List[A]] {
      def apply(index: Z): List[A] = {
        val w = ev.size
        @tailrec def loop(len: Int, i: Z): (Int, Z) = {
          val j = i + ev.size.pow(len)
          if (j <= index) loop(len + 1, j) else (len, index - i)
        }
        val (len, i) = loop(0, Z.zero)
        @tailrec def build(len: Int, i: Z, as: List[A]): List[A] =
          if (len <= 0) as
          else {
            val (j, k) = i /% ev.size
            build(len - 1, j, ev.get(k).get :: as)
          }
        build(len, i, Nil)
      }
    }

  // support case classes and tuples via generic derivation.
  implicit def cgeneric[A, H <: HList](implicit gen: Generic.Aux[A, H], evh: HInfinite[H]): Infinite[A] =
    new Infinite[A] {
      val size = evh.size
      def apply(index: Z): A =
        gen.from(evh.translate(Diagonal.atIndex(size, index)))
    }

  // we define this in terms of (H :: T) because Infinite[HNil] is
  // impossible.
  implicit def chlist[H, T <: HList](implicit evh: HInfinite[H :: T]): Infinite[H :: T] =
    new Infinite[H :: T] {
      val size = evh.size
      def apply(index: Z): H :: T =
        evh.translate(Diagonal.atIndex(size, index))
    }

  // helpful class for defining finite instances derived from integer ranges.
  case class Integers[A](size: Z, f: Z => A) extends Finite[A] {
    def get(index: Z): Option[A] =
      if (index >= size) None
      else if (index >= 0) Some(f(index))
      else sys.error("!")
  }

  // type class that witnesses to having a bunch of infinite
  // instances. for a type (A1 :: A2 :: ... An :: HNil) we'll have
  // Infinite[A1], Infinite[A2], ... Infinite[An].
  trait HInfinite[H <: HList] {
    def size: Int
    def translate(elem: List[Z]): H
  }

  object HInfinite{

    implicit object HINil extends HInfinite[HNil] {
      def size: Int = 0
      def translate(elem: List[Z]): HNil = {
        require(elem.isEmpty, elem.toString)
        HNil
      }
    }

    implicit def hicons[A, H <: HList](implicit eva: Infinite[A], evh: HInfinite[H]): HInfinite[A :: H] =
      new HInfinite[A :: H] {
        def size: Int = evh.size + 1
        def translate(elem: List[Z]): A :: H =
          eva(elem.head) :: evh.translate(elem.tail)
      }
  }
}
