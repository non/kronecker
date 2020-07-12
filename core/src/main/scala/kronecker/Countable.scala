package kronecker

import java.lang.Double.{doubleToRawLongBits, longBitsToDouble}
import java.lang.Float.{floatToRawIntBits, intBitsToFloat}
import kronecker.instances._
import scala.collection.mutable
import scala.reflect.ClassTag
import shapeless._
import spire.math.Searching

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

  /**
   * Drop the first k items from the countable instance.
   *
   * Note that the result is not a lawful countable instance, unless
   * the subset of A that remains is translated onto a new type.
   */
  def drop(k: Int): Countable[A] =
    new Countable[A] {
      require(k >= 0)
      def cardinality: Card = self.cardinality - k
      def get(index: Z): Option[A] = self.get(index + k)
    }

  /**
   * Permuate this countable's enumeration into a different
   * enumeration.
   *
   * This method uses a permutation on A values (the values being
   * counted).
   */
  def permute(p: Permutation[A]): Countable[A] =
    new Countable[A] {
      def cardinality: Card =
        self.cardinality
      def get(index: Z): Option[A] =
        self.get(index).map(p(_))
    }

  /**
   * Permuate this countable's enumeration into a different
   * enumeration.
   *
   * This method uses a permutation on numbers (the indices being
   * enumerated).
   */
  def copermute(p: Permutation[Z]): Countable[A] =
    new Countable[A] {
      def cardinality: Card =
        self.cardinality
      def get(index: Z): Option[A] =
        self.get(p(index))
    }
}

object Countable extends Countable1 {

  /**
   * Summon an implicit Countable[A] instance.
   */
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

  /**
   * Produce an "empty" countable with cardinality zero.
   */
  def empty[A]: Indexable[A] =
    new Indexable[A] {
      def cardinality: Card = Card.zero
      def get(index: Z): Option[A] = None
      def index(a: A): Z = sys.error("impossible")
    }

  /**
   * Produce a countable with exactly one value.
   */
  def singleton[A](a: A): Indexable[A] =
    new Indexable[A] {
      def cardinality: Card = Card.one
      def get(index: Z): Option[A] = if (index.isZero) Some(a) else None
      def index(a: A): Z = Z.zero
    }

  /**
   * Given a collection of non-overlapping countable instances, build
   * a new countable instance which includes all values produced by
   * any of the given instances.
   *
   * The given countable instances must be non-overlapping; this means
   * that they should not ever produce the same A values. If instances
   * were to overlap, the cardinality of the resulting instance would
   * be too large, and it would produce the same value for two or more
   * indices, violating the laws.
   */
  def oneOf[A](cs: Countable[A]*): Countable[A] =
    if (cs.isEmpty) {
      Countable.empty[A]
    } else if (cs.size == 1) {
      cs(0)
    } else {
      var ctot = Z.zero
      val cbuf = mutable.ArrayBuffer.empty[Z]
      val fbuf = mutable.ArrayBuffer.empty[Countable[A]]
      val ibuf = mutable.ArrayBuffer.empty[Countable[A]]

      cs.foreach { c =>
        c.cardinality match {
          case Card.Finite(n) =>
            ctot += n
            cbuf.append(ctot)
            fbuf.append(c)
          case _ =>
            ibuf.append(c)
        }
      }

      def allFinite: Countable[A] =
        new Countable[A] {
          val cardinality: Card = Card(ctot)
          val cutoffs: Array[Z] = cbuf.toArray
          val cs: Array[Countable[A]] = fbuf.toArray
          def get(index: Z): Option[A] =
            if (cardinality.contains(index)) {
              val i: Int = Searching.search(cutoffs, index)
              val j: Int = if (i < 0) -(i + 1) else i + 1
              val k: Z = if (j > 0) cutoffs(j - 1) else Z.zero
              cs(j).get(index - k)
            } else {
              None
            }
        }

      def allInfinite: Countable[A] =
        new Countable[A] {
          val cs = ibuf.toArray
          def cardinality: Card = Card.infinite
          def get(index: Z): Option[A] = {
            val (q, r) = index /% cs.length
            cs(r.toInt).get(q)
          }
        }

      (ibuf.nonEmpty, fbuf.nonEmpty) match {
        case (true, true) =>
          Countable.ceither(allFinite, allInfinite).translate {
            case Left(a) => a
            case Right(a) => a
          }
        case (false, true) =>
          allFinite
        case (true, false) =>
          allInfinite
        case (false, false) =>
          Countable.empty[A]
      }
    }

  // .
  implicit val cnothing: Indexable[Nothing] =
    empty[Nothing]

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

  /**
   * Double precision floating point
   *
   * sign: 1-bit
   * exponent: 11-bits
   * mantissa: 52-bits (with an implied leading one)
   *
   * value = (1 - 2*sign) * 2^(e - 1023) * (1+(mantissa/2^52))
   *
   * e=0x000 -> if (m=0) +/- zero else subnormal numbers
   * e=0x7ff -> if (m=0) +/- inf else NaN
   *
   * one future proposed enumeration:
   *
   *   0:   0.0   (0,0,0)
   *   1:   1.0   (0,1023,0)
   *   2:  -1.0   (1,1023,0)
   *   3:   0.5   (0,1023,512)
   *   4:  -0.5   (1,1023,512)
   *   5:   0.25  (0,1023,256)
   *   6:  -0.25  (1,1023,256)
   *   7:   0.75  (0,1023,768)
   *   8:  -0.75  (1,1023,768)
   *   ...
   *
   * alternately we could exhaust the powers of 2 at one mantissa
   * before moving on, or interleave these strategies.
   */

  implicit val cdouble: Indexable[Double] =
    clong.imap(longBitsToDouble)(doubleToRawLongBits)

  implicit val cbigInt: Indexable[BigInt] =
    cz.imap(_.toBigInt)(Z(_))

  implicit val cbigInteger: Indexable[java.math.BigInteger] =
    cz.imap(_.toBigInt.bigInteger)(Z(_))

  // Option: could also be provided generically
  implicit def coption[A](implicit ev: Countable[A]): Countable[Option[A]] =
    new COption(ev)
  implicit def noption[A](implicit ev: Indexable[A]): Indexable[Option[A]] =
    new NOption(ev)

  // Either: could also be provided generically
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

  implicit def icgeneric[A, C <: Coproduct](implicit gen: Generic.Aux[A, C], evc: CIndexable[C]): Indexable[A] =
    icoproduct(evc).imap(gen.from)(gen.to)

  implicit def icoproduct[C <: Coproduct](implicit evc: CIndexable[C]): Indexable[C] =
    new CIndexable.ToIndexable(evc)
}

abstract class Countable0 {

  implicit def chgeneric[A, H <: HList](implicit gen: Generic.Aux[A, H], evh: HCountable[H]): Countable[A] =
    chlist(evh).translate(gen.from)

  implicit def chlist[H <: HList](implicit evh: HCountable[H]): Countable[H] =
    new HCountable.ToCountable(evh)

  implicit def ccgeneric[A, C <: Coproduct](implicit gen: Generic.Aux[A, C], evc: CCountable[C]): Countable[A] =
    ccoproduct(evc).translate(gen.from)

  implicit def ccoproduct[C <: Coproduct](implicit evc: CCountable[C]): Countable[C] =
    new CCountable.ToCountable(evc)
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

  /**
   * Map an Indexable[A] into an Indexable[B].
   *
   * We require a bijection (represented as `f` and `g`), i.e. an
   * invertible function.
   */
  def imap[B](f: A => B)(g: B => A): Indexable[B] =
    new Indexable[B] {
      def cardinality: Card = self.cardinality
      def get(index: Z): Option[B] = self.get(index).map(f)
      def index(b: B): Z = self.index(g(b))
    }

  /**
   * Drop the first k items from the indexable instance.
   *
   * Note that the result is not a lawful countable instance, unless
   * the subset of A that remains is translated onto a new type.
   */
  override def drop(k: Int): Indexable[A] =
    new Indexable[A] {
      require(k >= 0)
      def cardinality: Card = self.cardinality - k
      def get(index: Z): Option[A] = self.get(index + k)
      def index(a: A): Z = self.index(a) - k
    }

  /**
   * Permuate this indexable's enumeration into a different
   * enumeration.
   *
   * This method uses a permutation on A values (the values being
   * counted).
   */
  override def permute(p: Permutation[A]): Indexable[A] =
    new Indexable[A] {
      def cardinality: Card =
        self.cardinality
      def get(index: Z): Option[A] =
        self.get(index).map(p(_))
      def index(a: A): Z =
        self.index(p(a))
    }

  /**
   * Permuate this indexable's enumeration into a different
   * enumeration.
   *
   * This method uses a permutation on numbers (the indices being
   * enumerated).
   */
  override def copermute(p: Permutation[Z]): Indexable[A] =
    new Indexable[A] {
      def cardinality: Card =
        self.cardinality
      def get(index: Z): Option[A] =
        self.get(p(index))
      def index(a: A): Z =
        p(self.index(a))
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
