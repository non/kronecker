package kronecker

import scala.annotation.tailrec

import java.lang.Double.longBitsToDouble
import java.lang.Float.intBitsToFloat
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
 *
 * There are two types of Countable instances:
 *
 *   - Finite: contains a fixed set of values
 *   - Infinite: contains an unbounded set of values
 *
 * Only one instance or the other should be defined for any given type
 * (either a type is Finite, or Infinite, but not both).
 */
sealed trait Countable[A] { self =>

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
  def translate[B](f: A => B): Countable[B]
}

/**
 * Countable[A] for finite sets of A values.
 */
trait Finite[A] extends Countable[A] { self =>
  def size: Z

  final def cardinality: Card = Card.Finite(size)

  def translate[B](f: A => B): Finite[B] =
    new Finite[B] {
      def size: Z = self.size
      def get(index: Z): Option[B] = self.get(index).map(f)
    }
}

/**
 * Countable[A] for infinite (unbounded) sets of A values.
 */
trait Infinite[A] extends Countable[A] { self =>
  def apply(index: Z): A

  final def cardinality: Card = Card.Infinite

  final def get(index: Z): Some[A] = Some(apply(index))

  def translate[B](f: A => B): Infinite[B] =
    new Infinite[B] {
      def apply(index: Z): B = f(self(index))
    }
}

object Countable extends Countable0 {

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

  // 0, 1, 2, ..., 65535, -65536, -65535, ..., -1.
  implicit val cchar: Finite[Char] =
    Integers(Z.one << 16, _.toChar)

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

  // None, Some(0), Some(1), ...
  implicit def foption[A](implicit ev: Finite[A]): Finite[Option[A]] =
    new Finite[Option[A]] {
      val size: Z = ev.size + 1
      def get(index: Z): Option[Option[A]] =
        if (index == 0) Some(None)
        else ev.get(index - 1).map(Some(_))
    }

  // None, Some(0), Some(1), ...
  implicit def ioption[A](implicit ev: Infinite[A]): Infinite[Option[A]] =
    new Infinite[Option[A]] {
      def apply(index: Z): Option[A] =
        if (index == 0) None else Some(ev(index - 1))
    }

  // Left(0), Left(1), ..., Right(0), Right(1), ...
  implicit def feither[A, B](implicit eva: Finite[A], evb: Finite[B]): Finite[Either[A, B]] =
    new Finite[Either[A, B]] {
      val size: Z = eva.size + evb.size
      def get(index: Z): Option[Either[A, B]] =
        if (index < eva.size) eva.get(index).map(Left(_))
        else evb.get(index - eva.size).map(Right(_))
    }

  // Left(0), Left(1), ..., Right(0), Right(1), ...
  implicit def fieither[A, B](implicit eva: Finite[A], evb: Infinite[B]): Infinite[Either[A, B]] =
    new Infinite[Either[A, B]] {
      def apply(index: Z): Either[A, B] =
        if (index < eva.size) Left(eva.get(index).get)
        else Right(evb(index - eva.size))
    }

  // Right(0), Right(1), ... Left(0), Left(1), ...
  implicit def ifeither[A, B](implicit eva: Infinite[A], evb: Finite[B]): Infinite[Either[A, B]] =
    fieither[B, A](evb, eva).translate {
      case Left(b) => Right(b)
      case Right(a) => Left(a)
    }

  // Left(0), Right(0), Left(1), Right(1), ...
  implicit def ieither[A, B](implicit eva: Infinite[A], evb: Infinite[B]): Infinite[Either[A, B]] =
    new Infinite[Either[A, B]] {
      def apply(index: Z): Either[A, B] = {
        val i = index >> 1
        if (index.isEven) Left(eva(i)) else Right(evb(i))
      }
    }

  val MaxExponent = Z(2).pow(65536)

  // Set(), Set(0), Set(1), Set(0, 1), Set(2), Set(0, 2), Set(1, 2),
  // Set(0, 1, 2), Set(3), ...
  implicit def fset[A](implicit ev: Finite[A]): Finite[Set[A]] =
    new Finite[Set[A]] {
      // NOTE: powOf() will crash for unsupportedly-large cardinalities
      val size: Z = powOf(Z(2), ev.size)
      def get(index: Z): Option[Set[A]] = {
        @tailrec def loop(rem: Z, index: Z, s0: Set[A]): Set[A] =
          if (rem.isZero || index >= ev.size) s0
          else {
            val s1 = if (rem.isOdd) s0 + ev.get(index).get else s0
            loop(rem >> 1, index + 1, s1)
          }
        if (index >= size) None
        else Some(loop(index, Z.zero, Set.empty))
      }
    }

  // Set(), Set(0), Set(1), Set(0, 1), Set(2), Set(0, 2), Set(1, 2),
  // Set(0, 1, 2), Set(3), ...
  implicit def iset[A](implicit ev: Infinite[A]): Infinite[Set[A]] =
    new Infinite[Set[A]] {
      def apply(index: Z): Set[A] = {
        @tailrec def loop(rem: Z, index: Z, s0: Set[A]): Set[A] =
          if (rem.isZero) s0
          else if (rem.isOdd) loop(rem >> 1, index + 1, s0 + ev(index))
          else loop(rem >> 1, index + 1, s0)
        loop(index, Z.zero, Set.empty)
      }
    }

  implicit def fmap[K, V](implicit evk: Finite[K], evv: Finite[V]): Finite[Map[K, V]] =
    new Finite[Map[K, V]] {
      val evo: Finite[Option[V]] = foption(evv)
      // NOTE: powOf() will crash for unsupportedly-large cardinalities
      val size: Z = powOf(evo.size, evk.size)
      def get(index: Z): Option[Map[K, V]] = {
        @tailrec def loop(index0: Z, keyIndex: Z, m0: Map[K, V]): Map[K, V] =
          if (index0.isZero || keyIndex >= evk.size) m0
          else {
            val (index1, valIndex) = index0 /% evo.size
            evo.get(valIndex).get match {
              case Some(v) =>
                val k = evk.get(keyIndex).get
                loop(index1, keyIndex + 1, m0.updated(k, v))
              case None =>
                loop(index1, keyIndex + 1, m0)
            }
          }
        if (index >= size) None
        else Some(loop(index, Z.zero, Map.empty))
      }
    }

  implicit def imap[K, V](implicit evk: Infinite[K], evv: Finite[V]): Infinite[Map[K, V]] =
    new Infinite[Map[K, V]] {
      val evo: Finite[Option[V]] = foption(evv)
      def apply(index: Z): Map[K, V] = {
        @tailrec def loop(index0: Z, keyIndex: Z, m0: Map[K, V]): Map[K, V] =
          if (index0.isZero) m0
          else {
            val (index1, valIndex) = index0 /% evo.size
            evo.get(valIndex).get match {
              case Some(v) =>
                val k = evk(keyIndex)
                loop(index1, keyIndex + 1, m0.updated(k, v))
              case None =>
                loop(index1, keyIndex + 1, m0)
            }
          }
        loop(index, Z.zero, Map.empty)
      }
    }

  implicit def ffunction[A, B](implicit ca: Finite[A], ia: Indexable[A], cb: Finite[B]): Finite[A => B] =
    new Finite[A => B] {
      // NOTE: powOf() will crash for unsupportedly-large cardinalities
      val size: Z = powOf(cb.size, ca.size)
      def get(index: Z): Option[A => B] =
        if (index >= size) None
        else {
          val f = Functional.function1(index, cb.size)
          Some((a: A) => cb.get(f(ia.index(a))).get)
        }
    }

  implicit def ifunction[A, B](implicit ca: Infinite[A], ia: Indexable[A], cb: Finite[B]): Infinite[A => B] =
    new Infinite[A => B] {
      def apply(index: Z): A => B = {
        val f = Functional.function1(index, cb.size)
        (a: A) => cb.get(f(ia.index(a))).get
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

  implicit def lexicographicVector[A](implicit ev: Finite[A]): Infinite[Vector[A]] =
    lexicographicList(ev).translate(_.toVector)

  // quite ugly
  implicit val cstring: Infinite[String] =
    lexicographicList[Char].translate(_.mkString)

  // helpful class for defining finite instances derived from integer ranges.
  case class Integers[A](size: Z, f: Z => A) extends Finite[A] {
    def get(index: Z): Option[A] =
      if (index >= size) None
      else if (index >= 0) Some(f(index))
      else sys.error("!")
  }
}

abstract class Countable0 extends Countable1 {

  // support case classes and tuples via generic derivation.
  implicit def fhgeneric[A, H <: HList](implicit gen: Generic.Aux[A, H], evh: HFinite[H]): Finite[A] =
    fhlist(evh).translate(gen.from)

  // support case classes and tuples via generic derivation.
  implicit def fcgeneric[A, C <: Coproduct](implicit gen: Generic.Aux[A, C], evc: CFinite[C]): Finite[A] =
    fcoproduct(evc).translate(gen.from)

  // suport Hlists
  implicit def fhlist[H <: HList](implicit evh: HFinite[H]): Finite[H] =
    new Finite[H] {
      def size = evh.size
      def get(index: Z): Option[H] = evh.get(index)
    }

  // suport Coproducts
  implicit def fcoproduct[C <: Coproduct](implicit evc: CFinite[C]): Finite[C] =
    new Finite[C] {
      def size = evc.size
      def get(index: Z): Option[C] = evc.get(index)
    }
}

abstract class Countable1 extends Countable2 {

  // support case classes and tuples via generic derivation.
  implicit def ihgeneric[A, H <: HList](implicit gen: Generic.Aux[A, H], evh: HInfinite[H]): Infinite[A] =
    ihlist(evh).translate(gen.from)

  // support case classes and tuples via generic derivation.
  implicit def icgeneric[A, C <: Coproduct](implicit gen: Generic.Aux[A, C], evc: CInfinite[C]): Infinite[A] =
    icoproduct(evc).translate(gen.from)

  // suport Hlists
  implicit def ihlist[H <: HList](implicit evh: HInfinite[H]): Infinite[H] =
    new Infinite[H] {
      val arity = evh.arity
      def apply(index: Z): H =
        evh.lookup(Diagonal.atIndex(arity, index))
    }

  // suport Coproducts
  implicit def icoproduct[C <: Coproduct](implicit evc: CInfinite[C]): Infinite[C] =
    new Infinite[C] {
      def apply(index: Z): C = evc(index)
    }
}

abstract class Countable2 {
  implicit def mhgeneric[A, H <: HList](implicit gen: Generic.Aux[A, H], evh: HMixed[H]): Infinite[A] =
    mhlist(evh).translate(gen.from)

  implicit def mhlist[H <: HList](implicit evh: HMixed[H]): Infinite[H] =
    new Infinite[H] {
      def apply(index: Z): H = evh.build(index)
    }

  implicit def mcgeneric[A, C <: Coproduct](implicit gen: Generic.Aux[A, C], evc: CMixed[C]): Infinite[A] =
    mcoproduct(evc).translate(gen.from)

  implicit def mcoproduct[C <: Coproduct](implicit evc: CMixed[C]): Infinite[C] =
    new Infinite[C] {
      def apply(index: Z): C = evc(index)
    }
}
