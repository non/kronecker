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
 *
 * There are two types of Countable instances:
 *
 *   - Finite: contains a fixed set of values
 *   - Infinite: contains an unbounded set of values
 *
 * Only one instance or the other should be defined for any given type
 * (either a type is Finite, or Infinite, but not both).
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

object Countable extends Countable0 {

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
    Indexable.FromRange[Nothing](Z.zero, _ => sys.error("impossible"))(_ => sys.error("impossible"))

  // ().
  implicit val cunit: Indexable[Unit] =
    Indexable.FromRange(Z.one, _ => ())(_ => Z.zero)

  // false, true.
  implicit val cboolean: Indexable[Boolean] =
    Indexable.FromRange(Z.two, _ == Z.one)(b => Z(if (b) 1 else 0))

  // TODO: the signed streams could be in a nicer order; right now the
  // negatives only come after you exhaust the positives.

  // 0, 1, 2, ..., 127, -128, -127, ..., -1.
  implicit val cbyte: Indexable[Byte] =
    Indexable.FromRange(Z.one << 8, _.toByte)(n => Z(n & 0xff))

  // 0, 1, 2, ..., 65535, -65536, -65535, ..., -1.
  implicit val cshort: Indexable[Short] =
    Indexable.FromRange(Z.one << 16, _.toShort)(n => Z(n & 0xffff))

  // 0, 1, 2, ..., 65535, -65536, -65535, ..., -1.
  implicit val cchar: Indexable[Char] =
    Indexable.FromRange(Z.one << 16, _.toChar)(Z(_))

  // 0, 1, 2, ... -1.
  implicit val cint: Indexable[Int] =
    Indexable.FromRange(Z.one << 32, _.toInt)(n => Z(n & 0xffffffffL))

  val BeyondLong = Z.one << 64

  // 0, 1, 2, ... -1.
  implicit val clong: Indexable[Long] =
    Indexable.FromRange(BeyondLong, _.toLong)(n => if (n >= 0L) Z(n) else BeyondLong + n)

  // TODO: the floating point streams could be in a MUCH nicer order;
  // the current order is pretty much nonsense.

  implicit val cfloat: Indexable[Float] =
    Indexable.FromRange(Z.one << 32, n => intBitsToFloat(n.toInt))(x => Z(floatToRawIntBits(x)))

  implicit val cdouble: Indexable[Double] =
    Indexable.FromRange(Z.one << 64, n => longBitsToDouble(n.toLong))(x => Z(doubleToRawLongBits(x)))

  implicit val cbigInt: Indexable[BigInt] =
    cz.imap(_.toBigInt)(Z(_))

  implicit val cbigInteger: Indexable[java.math.BigInteger] =
    cz.imap(_.toBigInt.bigInteger)(Z(_))

  implicit def coption[A](implicit ev: Countable[A]): Countable[Option[A]] =
    new COption(ev)
  implicit def noption[A](implicit ev: Indexable[A]): Indexable[Option[A]] =
    new NOption(ev)

  implicit def ceither[A, B](implicit eva: Countable[A], evb: Countable[B]): Countable[Either[A, B]] =
    CEither(eva, evb)
  implicit def neither[A, B](implicit eva: Indexable[A], evb: Indexable[B]): Indexable[Either[A, B]] =
    NEither(eva, evb)

  implicit def cset[A](implicit ev: Countable[A]): Countable[Set[A]] =
    CSet(ev)
  implicit def nset[A](implicit ev: Indexable[A]): Indexable[Set[A]] =
    NSet(ev)

  // implicit def ffmap[K, V](implicit evk: Finite[K], evv: Finite[V]): Finite[Map[K, V]] =
  //   FFMap(evk, evv)
  // implicit def ifmap[K, V](implicit evk: Infinite[K], evv: Finite[V]): Infinite[Map[K, V]] =
  //   IFMap(evk, evv)
  // implicit def fimap[K, V](implicit evk: Finite[K], evv: Infinite[V]): Infinite[Map[K, V]] =
  //   FIMap(evk, evv)
  // implicit def iimap[K, V](implicit evk: Infinite[K], evv: Infinite[V]): Infinite[Map[K, V]] =
  //   IIMap(evk, evv)

  implicit def cfunction[A, B](implicit eva: Indexable[A], evb: Countable[B]): Countable[A => B] =
    CFunction(eva, evb)

  implicit def clist[A](implicit ev: Countable[A]): Countable[List[A]] =
    CList(ev)
  implicit def nlist[A](implicit ev: Indexable[A]): Indexable[List[A]] =
    NList(ev)

  implicit def cvector[A](implicit ev: Countable[A]): Countable[Vector[A]] =
    CList(ev).translate(_.toVector)
  implicit def nvector[A](implicit ev: Indexable[A]): Indexable[Vector[A]] =
    NList(ev).imap(_.toVector)(_.toList)

  implicit def cstream[A](implicit ev: Countable[A]): Countable[Stream[A]] =
    CList(ev).translate(_.toStream)
  implicit def nstream[A](implicit ev: Indexable[A]): Indexable[Stream[A]] =
    NList(ev).imap(_.toStream)(_.toList)

  implicit def carray[A: ClassTag](implicit ev: Countable[A]): Countable[Array[A]] =
    CList(ev).translate(_.toArray)
  implicit def narray[A: ClassTag](implicit ev: Indexable[A]): Indexable[Array[A]] =
    NList(ev).imap(_.toArray)(_.toList)

  implicit val nstring: Indexable[String] =
    Indexable[List[Char]].imap(_.mkString)(_.toList)
}

abstract class Countable0 extends Countable1 {

  // // support case classes and tuples via generic derivation.
  // implicit def fhgeneric[A, H <: HList](implicit gen: Generic.Aux[A, H], evh: HFinite[H]): Countable.Finite[A] =
  //   fhlist(evh).translate(gen.from)
  // 
  // // support case classes and tuples via generic derivation.
  // implicit def fcgeneric[A, C <: Coproduct](implicit gen: Generic.Aux[A, C], evc: CFinite[C]): Countable.Finite[A] =
  //   fcoproduct(evc).translate(gen.from)
  // 
  // // suport Hlists
  // implicit def fhlist[H <: HList](implicit evh: HFinite[H]): Countable.Finite[H] =
  //   new Countable.Finite[H] {
  //     def size = evh.size
  //     def get(index: Z): Option[H] = evh.get(index)
  //   }
  // 
  // // suport Coproducts
  // implicit def fcoproduct[C <: Coproduct](implicit evc: CFinite[C]): Countable.Finite[C] =
  //   new Countable.Finite[C] {
  //     def size = evc.size
  //     def get(index: Z): Option[C] = evc.get(index)
  //   }
}

abstract class Countable1 extends Countable2 {

  // // support case classes and tuples via generic derivation.
  // implicit def ihgeneric[A, H <: HList](implicit gen: Generic.Aux[A, H], evh: HInfinite[H]): Countable.Infinite[A] =
  //   ihlist(evh).translate(gen.from)
  // 
  // // support case classes and tuples via generic derivation.
  // implicit def icgeneric[A, C <: Coproduct](implicit gen: Generic.Aux[A, C], evc: CInfinite[C]): Countable.Infinite[A] =
  //   icoproduct(evc).translate(gen.from)
  // 
  // // suport Hlists
  // implicit def ihlist[H <: HList](implicit evh: HInfinite[H]): Countable.Infinite[H] =
  //   new Countable.Infinite[H] {
  //     val arity = evh.arity
  //     def apply(index: Z): H =
  //       evh.lookup(Diagonal.atIndex(arity, index))
  //   }
  // 
  // // suport Coproducts
  // implicit def icoproduct[C <: Coproduct](implicit evc: CInfinite[C]): Countable.Infinite[C] =
  //   new Countable.Infinite[C] {
  //     def apply(index: Z): C = evc(index)
  //   }
}

abstract class Countable2 {
  // implicit def mhgeneric[A, H <: HList](implicit gen: Generic.Aux[A, H], evh: HMixed[H]): Countable.Infinite[A] =
  //   mhlist(evh).translate(gen.from)
  // 
  // implicit def mhlist[H <: HList](implicit evh: HMixed[H]): Countable.Infinite[H] =
  //   new Countable.Infinite[H] {
  //     def apply(index: Z): H = evh.build(index)
  //   }
  // 
  // implicit def mcgeneric[A, C <: Coproduct](implicit gen: Generic.Aux[A, C], evc: CMixed[C]): Countable.Infinite[A] =
  //   mcoproduct(evc).translate(gen.from)
  // 
  // implicit def mcoproduct[C <: Coproduct](implicit evc: CMixed[C]): Countable.Infinite[C] =
  //   new Countable.Infinite[C] {
  //     def apply(index: Z): C = evc(index)
  //   }
}

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

  // helpful class for defining finite instances derived from integer ranges.
  case class FromRange[A](size: Z, f: Z => A)(g: A => Z) extends Indexable[A] {
    val cardinality: Card = Card(size)
    def get(index: Z): Option[A] =
      if (index >= size) None
      else if (index >= 0) Some(f(index))
      else sys.error("!")
    def index(a: A): Z = g(a)
  }
}
