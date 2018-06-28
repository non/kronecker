package kronecker

import java.lang.Double.{doubleToRawLongBits, longBitsToDouble}
import java.lang.Float.{floatToRawIntBits, intBitsToFloat}
import kronecker.instances._
import scala.annotation.tailrec
import scala.reflect.ClassTag
import shapeless._

/**
 * Countable[A] represents an ordering of every possible A value.
 *
 * Particular values can be accessed by their index in this ordering.
 * Thus, get(0) returns the "first" A value, get(1) returns the
 * second, and so on.
 *
 * The values of type A might be a finite site, or an infinite set
 * (i.e. an unbounded set). In the latter case, this class must
 * diagonize the values so that every particular value has an index.
 *
 * It's possible to define "invalid" instances of Countable which
 * "miss" particular A values (and doesn't give a particular value
 * multiple indices). We have no way of verifying that an instance is
 * "valid" although the instances provided are believed to be valid.
 * We use law-checking to try to catch broken implementations.
 */
trait Countable[A] { self =>

  /**
   * Return the "size" of the set of all A values.
   */
  def cardinality: Card

  /**
   * Return the A value at the given index (if any).
   */
  def get(index: Z): Option[A]

  /**
   * Iterator over all A values.
   *
   * This iterator is mutable -- you should create a new iterator
   * every time you wish to step through all A values.
   */
  def iterator(): Iterator[A] =
    Iterator.from(0).map(self.get(_)).takeWhile(_.isDefined).map(_.get)

  /**
   * Lazy stream of all A values.
   *
   * Be careful! Scala memoizes streams, so if you hang onto the head
   * of this stream you could end up using a lot of memory.
   */
  def stream: Stream[A] =
    Stream.from(0).map(self.get(_)).takeWhile(_.isDefined).map(_.get)

  /**
   * Produce a new Countable[B] instance by translating this one using
   * the given function `f`.
   *
   * This function is not called `map` since in order to produce
   * "valid" Countable[B] values, the function `f` must be a
   * bijection. We have no way of verifying this, so the name
   * `translate` is used to hint that something unusual is going on.
   */
  def translate[B](f: A => B): Countable[B] =
    new Countable[B] {
      def cardinality: Card = self.cardinality
      def get(index: Z): Option[B] = self.get(index).map(f)
    }
}

object Countable extends Countable1 {

  def apply[A](implicit ev: Countable[A]): Countable[A] = ev

  // 0, 1, -1, 2, -2, ...
  implicit val cz: Indexable[Z] =
    new Indexable[Z] {

      def cardinality: Card = Card.Infinite

      def get(index: Z): Option[Z] = {
        val (quot, mod) = (index + 1) /% Z.two
        val sign = Z.one - (mod * Z.two)
        Some(sign * quot)
      }

      val minusTwo = Z(-2)

      def index(n: Z): Z =
        n.signum match {
          case 0 => Z.zero
          case x if x > 0 => n * Z.two - Z.one
          case _ => n * minusTwo
        }
    }

  // .
  implicit val cnothing: Indexable[Nothing] =
    Indexable.UnsignedRange[Nothing](Card.zero, _ => sys.error("impossible"))(_ => sys.error("impossible"))

  // ().
  implicit val cunit: Indexable[Unit] =
    Indexable.UnsignedRange(Card.one, _ => ())(_ => Z.zero)

  // false, true.
  implicit val cboolean: Indexable[Boolean] =
    Indexable.UnsignedRange(Card.two, _ == Z.one)(b => if (b) Z.one else Z.zero)

  // TODO: the signed streams could be in a nicer order; right now the
  // negatives only come after you exhaust the positives.

  // 0, 1, 2, ..., 127, -128, -127, ..., -1.
  implicit val cbyte: Indexable[Byte] =
    Indexable.SignedRange(Card(Z.one << 8), _.toByte)(Z(_))

  // 0, 1, 2, ..., 65535, -65536, -65535, ..., -1.
  implicit val cshort: Indexable[Short] =
    Indexable.SignedRange(Card(Z.one << 16), _.toShort)(Z(_))

  // 0, 1, 2, ..., 65535, -65536, -65535, ..., -1.
  implicit val cchar: Indexable[Char] =
    Indexable.UnsignedRange(Card(Z.one << 16), _.toChar)(Z(_))

  // 0, 1, 2, ... -1.
  implicit val cint: Indexable[Int] =
    Indexable.SignedRange(Card(Z.one << 32), _.toInt)(Z(_))

  // 0, 1, 2, ... -1.
  implicit val clong: Indexable[Long] =
    Indexable.SignedRange(Card(Z.one << 64), _.toLong)(Z(_))

  // TODO: the floating point streams could be in a MUCH nicer order;
  // the current order is pretty much nonsense.

  implicit val cfloat: Indexable[Float] =
    cint.imap(intBitsToFloat)(floatToRawIntBits)

  implicit val cdouble: Indexable[Double] =
    clong.imap(longBitsToDouble)(doubleToRawLongBits)

  implicit val cbigInt: Indexable[BigInt] =
    cz.imap(_.toBigInt)(Z(_))

  implicit val cbigInteger: Indexable[java.math.BigInteger] =
    cz.imap(_.toBigInt.bigInteger)(Z(_))

  // Option
  implicit def coption[A](implicit ev: Countable[A]): Countable[Option[A]] =
    new COption(ev)
  implicit def noption[A](implicit ev: Indexable[A]): Indexable[Option[A]] =
    new NOption(ev)

  // Either
  implicit def ceither[A, B](implicit eva: Countable[A], evb: Countable[B]): Countable[Either[A, B]] =
    CEither(eva, evb)
  implicit def neither[A, B](implicit eva: Indexable[A], evb: Indexable[B]): Indexable[Either[A, B]] =
    NEither(eva, evb)

  // Set
  implicit def cset[A](implicit ev: Countable[A]): Countable[Set[A]] =
    CSet(ev)
  implicit def nset[A](implicit ev: Indexable[A]): Indexable[Set[A]] =
    NSet(ev)

  // Map
  implicit def cmap[K, V](implicit evk: Countable[K], evv: Countable[V]): Countable[Map[K, V]] =
    CMap(evk, evv)

  // Function1
  implicit def cfunction1[A, B](implicit eva: Indexable[A], evb: Countable[B]): Countable[A => B] =
    CFunction1(eva, evb)

  // List
  implicit def clist[A](implicit ev: Countable[A]): Countable[List[A]] =
    CList(ev)
  implicit def nlist[A](implicit ev: Indexable[A]): Indexable[List[A]] =
    NList(ev)

  // Vector
  implicit def cvector[A](implicit ev: Countable[A]): Countable[Vector[A]] =
    CList(ev).translate(_.toVector)
  implicit def nvector[A](implicit ev: Indexable[A]): Indexable[Vector[A]] =
    NList(ev).imap(_.toVector)(_.toList)

  // Stream
  implicit def cstream[A](implicit ev: Countable[A]): Countable[Stream[A]] =
    CList(ev).translate(_.toStream)
  implicit def nstream[A](implicit ev: Indexable[A]): Indexable[Stream[A]] =
    NList(ev).imap(_.toStream)(_.toList)

  // Array
  implicit def carray[A: ClassTag](implicit ev: Countable[A]): Countable[Array[A]] =
    CList(ev).translate(_.toArray)
  implicit def narray[A: ClassTag](implicit ev: Indexable[A]): Indexable[Array[A]] =
    NList(ev).imap(_.toArray)(_.toList)

  // String
  implicit val nstring: Indexable[String] =
    Indexable[List[Char]].imap(_.mkString)(_.toList)
}

abstract class Countable1 extends Countable0 {

  implicit def ihgeneric[A, H <: HList](implicit gen: Generic.Aux[A, H], evh: HIndexable[H]): Indexable[A] =
    ihlist(evh).imap(gen.from)(gen.to)

  implicit def ihlist[H <: HList](implicit evh: HIndexable[H]): Indexable[H] =
    new HIndexable.ToIndexable(evh)
}

abstract class Countable0 {

  implicit def chgeneric[A, H <: HList](implicit gen: Generic.Aux[A, H], evh: HCountable[H]): Countable[A] =
    chlist(evh).translate(gen.from)

  implicit def chlist[H <: HList](implicit evh: HCountable[H]): Countable[H] =
    new HCountable.ToCountable(evh)

  implicit def ccgeneric[A, C <: Coproduct](implicit gen: Generic.Aux[A, C], evc: CCountable[C]): Countable[A] =
    ccoproduct(evc).translate(gen.from)

  implicit def ccoproduct[C <: Coproduct](implicit evc: CCountable[C]): Countable[C] =
    CCoproduct[C](evc)
}

/**
 * Indexable[A] represents Countable[A] instances which are
 * invertible.
 *
 * In theory any Countable[A] instance should be invertible (we should
 * be able to reverse the procedure). In practice, some values we
 * generate are too large or opaque to be able to reverse in general
 * (for example functions). In other cases the user may not be able to
 * take the time to define the instance.
 *
 * Indexable instances are very useful for testing the correctness of
 * Countable instances, so users are encouraged to prefer Indexable
 * where possible.
 */
trait Indexable[A] extends Countable[A] { self =>
  def index(a: A): Z

  def imap[B](f: A => B)(g: B => A): Indexable[B] =
    new Indexable[B] {
      def cardinality: Card = self.cardinality
      def get(index: Z): Option[B] = self.get(index).map(f)
      def index(b: B): Z = self.index(g(b))
    }
}

object Indexable {

  def apply[A](implicit ev: Indexable[A]): Indexable[A] = ev

  case class SignedRange[A](cardinality: Card, f: Z => A)(g: A => Z) extends Indexable[A] {
    def get(index: Z): Option[A] =
      if (!cardinality.contains(index)) None
      else Countable.cz.get(index).map(n => f(-n))
    def index(a: A): Z =
      Countable.cz.index(-g(a))
  }

  // helpful class for defining finite instances derived from integer ranges.
  case class UnsignedRange[A](cardinality: Card, f: Z => A)(g: A => Z) extends Indexable[A] {
    def get(index: Z): Option[A] =
      if (!cardinality.contains(index)) None
      else Some(f(index))
    def index(a: A): Z =
      g(a)
  }
}
