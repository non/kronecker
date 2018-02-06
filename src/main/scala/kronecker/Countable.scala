package kronecker

import scala.annotation.tailrec

import java.lang.Double.longBitsToDouble
import java.lang.Float.intBitsToFloat

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

  implicit def tuple2[A, B](implicit eva: Infinite[A], evb: Infinite[B]): Infinite[(A, B)] =
    new Infinite[(A, B)] {
      def apply(index: Z): (A, B) = {
        val List(x, y) = Diagonal.atIndex(2, index)
        (eva(x), evb(y))
      }
    }

  implicit def tuple3[A, B, C](implicit eva: Infinite[A], evb: Infinite[B], evc: Infinite[C]): Infinite[(A, B, C)] =
    new Infinite[(A, B, C)] {
      def apply(index: Z): (A, B, C) = {
        val List(x, y, z) = Diagonal.atIndex(3, index)
        (eva(x), evb(y), evc(z))
      }
    }

  case class Integers[A](size: Z, f: Z => A) extends Finite[A] {
    def get(index: Z): Option[A] =
      if (index >= size) None
      else if (index >= 0) Some(f(index))
      else sys.error("!")
  }
}
