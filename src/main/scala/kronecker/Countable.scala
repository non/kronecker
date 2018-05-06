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

object Countable extends Countable0 {

  def apply[A](implicit ev: Countable[A]): Countable[A] = ev
  def finite[A](implicit ev: Countable.Finite[A]): Countable.Finite[A] = ev
  def infinite[A](implicit ev: Countable.Infinite[A]): Countable.Infinite[A] = ev

  /**
   * Countable[A] for finite sets of A values.
   */
  trait Finite[A] extends Countable[A] { self =>
    def size: Z

    final def cardinality: Card = Card.Finite(size)

    final def translate[B](f: A => B): Finite[B] =
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

    final def translate[B](f: A => B): Infinite[B] =
      new Infinite[B] {
        def apply(index: Z): B = f(self(index))
      }
  }

  // 0, 1, -1, 2, -2, ...
  implicit val cz: Indexable.Infinite[Z] =
    new Indexable.Infinite[Z] {
      def apply(index: Z): Z = {
        val (quot, mod) = (index + 1) /% Z.two
        val sign = Z.one - (mod * Z.two)
        sign * quot
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
  implicit val cnothing: Indexable.Finite[Nothing] =
    Indexable.FromRange[Nothing](Z.zero, _ => sys.error("impossible"))(_ => sys.error("impossible"))

  // ().
  implicit val cunit: Indexable.Finite[Unit] =
    Indexable.FromRange(Z.one, _ => ())(_ => Z.zero)

  // false, true.
  implicit val cboolean: Indexable.Finite[Boolean] =
    Indexable.FromRange(Z.two, _ == Z.one)(b => Z(if (b) 1 else 0))

  // TODO: the signed streams could be in a nicer order; right now the
  // negatives only come after you exhaust the positives.

  // 0, 1, 2, ..., 127, -128, -127, ..., -1.
  implicit val cbyte: Indexable.Finite[Byte] =
    Indexable.FromRange(Z.one << 8, _.toByte)(n => Z(n & 0xff))

  // 0, 1, 2, ..., 65535, -65536, -65535, ..., -1.
  implicit val cshort: Indexable.Finite[Short] =
    Indexable.FromRange(Z.one << 16, _.toShort)(n => Z(n & 0xffff))

  // 0, 1, 2, ..., 65535, -65536, -65535, ..., -1.
  implicit val cchar: Indexable.Finite[Char] =
    Indexable.FromRange(Z.one << 16, _.toChar)(Z(_))

  // 0, 1, 2, ... -1.
  implicit val cint: Indexable.Finite[Int] =
    Indexable.FromRange(Z.one << 32, _.toInt)(n => Z(n & 0xffffffffL))

  val BeyondLong = Z.one << 64

  // 0, 1, 2, ... -1.
  implicit val clong: Indexable.Finite[Long] =
    Indexable.FromRange(BeyondLong, _.toLong)(n => if (n >= 0L) Z(n) else BeyondLong + n)

  // TODO: the floating point streams could be in a MUCH nicer order;
  // the current order is pretty much nonsense.

  implicit val cfloat: Indexable.Finite[Float] =
    Indexable.FromRange(Z.one << 32, n => intBitsToFloat(n.toInt))(x => Z(floatToRawIntBits(x)))

  implicit val cdouble: Indexable.Finite[Double] =
    Indexable.FromRange(Z.one << 64, n => longBitsToDouble(n.toLong))(x => Z(doubleToRawLongBits(x)))

  implicit val cbigInt: Indexable.Infinite[BigInt] =
    cz.imap(_.toBigInt)(Z(_))

  implicit val cbigInteger: Indexable.Infinite[java.math.BigInteger] =
    cz.imap(_.toBigInt.bigInteger)(Z(_))

  implicit def cfoption[A](implicit ev: Countable.Finite[A]): Countable.Finite[Option[A]] =
    new CFOption(ev)
  implicit def cioption[A](implicit ev: Countable.Infinite[A]): Countable.Infinite[Option[A]] =
    new CIOption(ev)

  implicit def ifoption[A](implicit ev: Indexable.Finite[A]): Indexable.Finite[Option[A]] =
    new NFOption(ev)
  implicit def iioption[A](implicit ev: Indexable.Infinite[A]): Indexable.Infinite[Option[A]] =
    new NIOption(ev)

  implicit def cffeither[A, B](implicit eva: Countable.Finite[A], evb: Countable.Finite[B]): Countable.Finite[Either[A, B]] =
    new CFFEither(eva, evb)
  implicit def cfieither[A, B](implicit eva: Countable.Finite[A], evb: Countable.Infinite[B]): Countable.Infinite[Either[A, B]] =
    new CFIEither(eva, evb)
  implicit def ciieither[A, B](implicit eva: Countable.Infinite[A], evb: Countable.Infinite[B]): Countable.Infinite[Either[A, B]] =
    new CIIEither(eva, evb)
  implicit def cifeither[A, B](implicit eva: Countable.Infinite[A], evb: Countable.Finite[B]): Countable.Infinite[Either[A, B]] =
    cfieither(evb, eva).translate {
      case Left(b) => Right(b)
      case Right(a) => Left(a)
    }

  implicit def nffeither[A, B](implicit eva: Indexable.Finite[A], evb: Indexable.Finite[B]): Indexable.Finite[Either[A, B]] =
    new NFFEither(eva, evb)
  implicit def nfieither[A, B](implicit eva: Indexable.Finite[A], evb: Indexable.Infinite[B]): Indexable.Infinite[Either[A, B]] =
    new NFIEither(eva, evb)
  implicit def niieither[A, B](implicit eva: Indexable.Infinite[A], evb: Indexable.Infinite[B]): Indexable.Infinite[Either[A, B]] =
    new NIIEither(eva, evb)
  implicit def nifeither[A, B](implicit eva: Indexable.Infinite[A], evb: Indexable.Finite[B]): Indexable.Infinite[Either[A, B]] =
    nfieither(evb, eva).imap({
      case Left(b) => Right(b)
      case Right(a) => Left(a)
    })({
      case Left(b) => Right(b)
      case Right(a) => Left(a)
    })

  implicit def cfset[A](implicit ev: Countable.Finite[A]): Countable.Finite[Set[A]] =
    new CFSet(ev)
  implicit def ciset[A](implicit ev: Countable.Infinite[A]): Countable.Infinite[Set[A]] =
    new CISet(ev)
  implicit def nfset[A](implicit ev: Indexable.Finite[A]): Indexable.Finite[Set[A]] =
    new NFSet(ev)
  implicit def niset[A](implicit ev: Indexable.Infinite[A]): Indexable.Infinite[Set[A]] =
    new NISet(ev)

  implicit def ffmap[K, V](implicit evk: Finite[K], evv: Finite[V]): Finite[Map[K, V]] =
    FFMap(evk, evv)
  implicit def ifmap[K, V](implicit evk: Infinite[K], evv: Finite[V]): Infinite[Map[K, V]] =
    IFMap(evk, evv)
  implicit def fimap[K, V](implicit evk: Finite[K], evv: Infinite[V]): Infinite[Map[K, V]] =
    FIMap(evk, evv)
  implicit def iimap[K, V](implicit evk: Infinite[K], evv: Infinite[V]): Infinite[Map[K, V]] =
    IIMap(evk, evv)

  implicit def fffunction[A, B](implicit ca: Finite[A], ia: Indexable[A], cb: Finite[B]): Finite[A => B] =
    FFFunction(ca, ia, cb)
  implicit def iffunction[A, B](implicit ca: Infinite[A], ia: Indexable[A], cb: Finite[B]): Infinite[A => B] =
    IFFunction(ca, ia, cb)
  // implicit def xifunction[A, B](implicit ia: Indexable[A], cb: Infinite[B]): Infinite[A => B] =
  //   XIFunction(ia, cb)

  // this only works if the A type is finite. A types that are
  // infinite require a different strategy than the lexicographic
  // order (since you can never "finish" the length=1 lists you have
  // to interleave larger ones).

  implicit def lexicographicList[A](implicit ev: Finite[A]): Infinite[List[A]] =
    new LexicographicList(ev)
  implicit def lexicographicVector[A](implicit ev: Finite[A]): Infinite[Vector[A]] =
    new LexicographicList(ev).translate(_.toVector)
  implicit def lexicographicStream[A](implicit ev: Finite[A]): Infinite[Stream[A]] =
    new LexicographicList(ev).translate(_.toStream)
  implicit def lexicographicArray[A](implicit ev: Finite[A], ct: ClassTag[A]): Infinite[Array[A]] =
    new LexicographicList(ev).translate(_.toArray)

  implicit def nlexicographicList[A](implicit ev: Indexable.Finite[A]): Indexable.Infinite[List[A]] =
    new NLexicographicList(ev)
  implicit def nlexicographicVector[A](implicit ev: Indexable.Finite[A]): Indexable.Infinite[Vector[A]] =
    new NLexicographicList(ev).imap(_.toVector)(_.toList)
  implicit def nlexicographicStream[A](implicit ev: Indexable.Finite[A]): Indexable.Infinite[Stream[A]] =
    new NLexicographicList(ev).imap(_.toStream)(_.toList)
  implicit def nlexicographicArray[A](implicit ev: Indexable.Finite[A], ct: ClassTag[A]): Indexable.Infinite[Array[A]] =
    new NLexicographicList(ev).imap(_.toArray)(_.toList)

  implicit def codedList[A](implicit ev: Infinite[A]): Infinite[List[A]] =
    new CodedList(ev)
  implicit def codedVector[A](implicit ev: Infinite[A]): Infinite[Vector[A]] =
    new CodedList(ev).translate(_.toVector)
  implicit def codedStream[A](implicit ev: Infinite[A]): Infinite[Stream[A]] =
    new CodedList(ev).translate(_.toStream)
  implicit def codedArray[A](implicit ev: Infinite[A], ct: ClassTag[A]): Infinite[Array[A]] =
    new CodedList(ev).translate(_.toArray)

  implicit def ncodedList[A](implicit ev: Indexable.Infinite[A]): Indexable.Infinite[List[A]] =
    new NCodedList(ev)
  implicit def ncodedVector[A](implicit ev: Indexable.Infinite[A]): Indexable.Infinite[Vector[A]] =
    new NCodedList(ev).imap(_.toVector)(_.toList)
  implicit def ncodedStream[A](implicit ev: Indexable.Infinite[A]): Indexable.Infinite[Stream[A]] =
    new NCodedList(ev).imap(_.toStream)(_.toList)
  implicit def ncodedArray[A](implicit ev: Indexable.Infinite[A], ct: ClassTag[A]): Indexable.Infinite[Array[A]] =
    new NCodedList(ev).imap(_.toArray)(_.toList)

  // quite ugly
  implicit val cstring: Indexable.Infinite[String] =
    new NLexicographicList(cchar).imap(_.mkString)(_.toList)
}

abstract class Countable0 extends Countable1 {

  // support case classes and tuples via generic derivation.
  implicit def fhgeneric[A, H <: HList](implicit gen: Generic.Aux[A, H], evh: HFinite[H]): Countable.Finite[A] =
    fhlist(evh).translate(gen.from)

  // support case classes and tuples via generic derivation.
  implicit def fcgeneric[A, C <: Coproduct](implicit gen: Generic.Aux[A, C], evc: CFinite[C]): Countable.Finite[A] =
    fcoproduct(evc).translate(gen.from)

  // suport Hlists
  implicit def fhlist[H <: HList](implicit evh: HFinite[H]): Countable.Finite[H] =
    new Countable.Finite[H] {
      def size = evh.size
      def get(index: Z): Option[H] = evh.get(index)
    }

  // suport Coproducts
  implicit def fcoproduct[C <: Coproduct](implicit evc: CFinite[C]): Countable.Finite[C] =
    new Countable.Finite[C] {
      def size = evc.size
      def get(index: Z): Option[C] = evc.get(index)
    }
}

abstract class Countable1 extends Countable2 {

  // support case classes and tuples via generic derivation.
  implicit def ihgeneric[A, H <: HList](implicit gen: Generic.Aux[A, H], evh: HInfinite[H]): Countable.Infinite[A] =
    ihlist(evh).translate(gen.from)

  // support case classes and tuples via generic derivation.
  implicit def icgeneric[A, C <: Coproduct](implicit gen: Generic.Aux[A, C], evc: CInfinite[C]): Countable.Infinite[A] =
    icoproduct(evc).translate(gen.from)

  // suport Hlists
  implicit def ihlist[H <: HList](implicit evh: HInfinite[H]): Countable.Infinite[H] =
    new Countable.Infinite[H] {
      val arity = evh.arity
      def apply(index: Z): H =
        evh.lookup(Diagonal.atIndex(arity, index))
    }

  // suport Coproducts
  implicit def icoproduct[C <: Coproduct](implicit evc: CInfinite[C]): Countable.Infinite[C] =
    new Countable.Infinite[C] {
      def apply(index: Z): C = evc(index)
    }
}

abstract class Countable2 {
  implicit def mhgeneric[A, H <: HList](implicit gen: Generic.Aux[A, H], evh: HMixed[H]): Countable.Infinite[A] =
    mhlist(evh).translate(gen.from)

  implicit def mhlist[H <: HList](implicit evh: HMixed[H]): Countable.Infinite[H] =
    new Countable.Infinite[H] {
      def apply(index: Z): H = evh.build(index)
    }

  implicit def mcgeneric[A, C <: Coproduct](implicit gen: Generic.Aux[A, C], evc: CMixed[C]): Countable.Infinite[A] =
    mcoproduct(evc).translate(gen.from)

  implicit def mcoproduct[C <: Coproduct](implicit evc: CMixed[C]): Countable.Infinite[C] =
    new Countable.Infinite[C] {
      def apply(index: Z): C = evc(index)
    }
}

sealed trait Indexable[A] extends Countable[A] { self =>
  def index(a: A): Z

  def imap[B](f: A => B)(g: B => A): Indexable[B]
}

object Indexable {

  def apply[A](implicit ev: Indexable[A]): Indexable[A] = ev
  def finite[A](implicit ev: Indexable.Finite[A]): Indexable.Finite[A] = ev
  def infinite[A](implicit ev: Indexable.Infinite[A]): Indexable.Infinite[A] = ev

  trait Finite[A] extends Indexable[A] with Countable.Finite[A] { self =>

    final def imap[B](f: A => B)(g: B => A): Indexable.Finite[B] =
      new Finite[B] {
        def size: Z = self.size
        def get(index: Z): Option[B] = self.get(index).map(f)
        def index(b: B): Z = self.index(g(b))
      }
  }

  trait Infinite[A] extends Indexable[A] with Countable.Infinite[A] { self =>
    final def imap[B](f: A => B)(g: B => A): Indexable.Infinite[B] =
      new Infinite[B] {
        def apply(index: Z): B = f(self(index))
        def index(b: B): Z = self.index(g(b))
      }
  }

  // helpful class for defining finite instances derived from integer ranges.
  case class FromRange[A](size: Z, f: Z => A)(g: A => Z) extends Finite[A] {
    def get(index: Z): Option[A] =
      if (index >= size) None
      else if (index >= 0) Some(f(index))
      else sys.error("!")
    def index(a: A): Z = g(a)
  }
}
