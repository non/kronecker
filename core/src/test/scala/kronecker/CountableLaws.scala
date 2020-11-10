package kronecker

import cats.kernel.Eq
import java.lang.Double.doubleToRawLongBits
import java.lang.Float.floatToRawIntBits
import org.scalacheck.{Arbitrary, Prop, Properties}
import org.scalacheck.Prop.{forAllNoShrink => forAll}
import scala.reflect.runtime.universe.TypeTag
import shapeless._

// we need to avoid the long/double instances
import cats.kernel.instances.boolean._
import cats.kernel.instances.byte._
import cats.kernel.instances.char._
import cats.kernel.instances.either._
import cats.kernel.instances.int._
import cats.kernel.instances.list._
import cats.kernel.instances.long._
import cats.kernel.instances.map._
import cats.kernel.instances.option._
import cats.kernel.instances.set._
import cats.kernel.instances.short._
import cats.kernel.instances.string._
import cats.kernel.instances.tuple._
import cats.kernel.instances.unit._
import cats.kernel.instances.vector._

import Testing._

trait CountableLaws[A] { self: Properties =>

  implicit def equiv: Eq[A]

  def ev: Countable[A]
  def tt: TypeTag[A]

  def eqv[B](b1: B, b2: B)(implicit eb: Eq[B]): Boolean =
    eb.eqv(b1, b2)

  val card = ev.cardinality

  property("get(i) is defined iff i < cardinality") =
    forAll { (i: Z) =>
      card.contains(i) == ev.get(i).isDefined
    }

  property("get(i) = get(j) iff i = j") = {
    forAll { (i1: Z, i2: Z) =>
      val (o1, o2) = (ev.get(i1), ev.get(i2))
      (card.contains(i1), card.contains(i2)) match {
        case (true, true) =>
          val lhs = eqv(o1.get, o2.get)
          val rhs = (i1 == i2)
          val res = lhs == rhs
          if (!res) println(s"card=$card i1=$i1 i2=$i2 o1=$o1 o2=$o2 lhs=$lhs rhs=$rhs res=$res")
          res
        case (true, false) =>
          val lhs = o1.isDefined
          val rhs = o2.isEmpty
          val res = lhs && rhs
          if (!res) println(s"card=$card i1=$i1 i2=$i2 o1=$o1 o2=$o2 lhs=$lhs rhs=$rhs res=$res")
          res
        case (false, true) =>
          val lhs = o1.isEmpty
          val rhs = o2.isDefined
          val res = lhs && rhs
          if (!res) println(s"card=$card i1=$i1 i2=$i2 o1=$o1 o2=$o2 lhs=$lhs rhs=$rhs res=$res")
          res
        case (false, false) =>
          val lhs = o1.isEmpty
          val rhs = o2.isEmpty
          val res = lhs && rhs
          if (!res) println(s"card=$card i1=$i1 i2=$i2 o1=$o1 o2=$o2 lhs=$lhs rhs=$rhs res=$res")
          res
      }
    }
  }
}

trait WeakIndexableLaws[A] extends CountableLaws[A] { self: Properties =>

  def ev: Indexable[A]

  def repr(n0: Z): String = {
    if (n0 == 0) return "0"
    var s = ""
    var n = n0
    while (n != 0) {
      val (q, r) = n /% 4
      s = r.toString + s
      n = q
    }
    s
  }

  property("sanity check") = {
    val ns = (0 to 100)
      .flatMap { i => ev.get(Z(i)).map(a => (i, a, ev.index(a))) }
      .filter { case (i, a, n) => i != n }
      .map { case (i, a, n) =>
        val a2 = ev.get(n).get
        s"i=$i (${repr(i)}) a=$a -> i2=$n (${repr(n)}) a2=$a2"
      }
      .toList
    Prop(ns.isEmpty) :| s"$ns should be empty"
  }


  property("get(i).forall(index(_) == i)") =
    forAll { (i: Z) =>
      ev.get(i).forall(a => ev.index(a) == i)
    }
}

trait StrongIndexableLaws[A] extends WeakIndexableLaws[A] { self: Properties =>

  implicit def arb: Arbitrary[A]

  property("0 <= index(a) < cardinality") =
    forAll { (a: A) =>
      card.contains(ev.index(a))
    }

  property("get(index(a)) = Some(a)") =
    forAll { (a: A) =>
      val i = ev.index(a)
      val o = ev.get(i)
      //val ok = o == Option(a)
      val ok = eqv(o, Option(a))
      if (!ok) println(s"a=$a i=$i o=$o")
      ok
    }

  property("index(a1) = index(a2) iff a1 = a2") =
    forAll { (a1: A, a2: A) =>
      val i1 = ev.index(a1)
      val i2 = ev.index(a2)
      //val ok = (i1 == i2) == (a1 == a2)
      val ok = (i1 == i2) == eqv(a1, a2)
      if (!ok) println(s"a1=$a1, a2=$a2, i1=$i1, i2=$i2")
      ok
    }
}

abstract class CountableTests[A](implicit val equiv: Eq[A], val ev: Countable[A], val tt: TypeTag[A])
    extends Properties(s"CountableTests[${tt.tpe}]") with CountableLaws[A]

abstract class WeakIndexableTests[A](implicit val equiv: Eq[A], val ev: Indexable[A], val tt: TypeTag[A])
    extends Properties(s"WeakIndexableTests[${tt.tpe}]") with WeakIndexableLaws[A]

abstract class StrongIndexableTests[A](implicit val equiv: Eq[A], val ev: Indexable[A], val arb: Arbitrary[A], val tt: TypeTag[A])
    extends Properties(s"StrongIndexableTests[${tt.tpe}]") with StrongIndexableLaws[A]

object EqInstances {

  // we want particular bit patterns of NaNs to equal themselves

  implicit val nanEqDouble: Eq[Double] =
    new Eq[Double] {
      def eqv(x: Double, y: Double): Boolean =
        doubleToRawLongBits(x) == doubleToRawLongBits(y)
    }

  implicit val nanEqFloat: Eq[Float] =
    new Eq[Float] {
      def eqv(x: Float, y: Float): Boolean =
        floatToRawIntBits(x) == floatToRawIntBits(y)
    }

  implicit val eqNothing: Eq[Nothing] =
    Eq.fromUniversalEquals

  implicit val eqHNil: Eq[HNil] =
    new Eq[HNil] {
      def eqv(x: HNil, y: HNil): Boolean = true
    }

  implicit def eqHCons[A, H <: HList](implicit eqa: Eq[A], eqh: Eq[H]): Eq[A :: H] =
    new Eq[A :: H] {
      def eqv(x: A :: H, y: A :: H): Boolean =
        eqa.eqv(x.head, y.head) && eqh.eqv(x.tail, y.tail)
    }

  implicit val eqCNil: Eq[CNil] =
    new Eq[CNil] {
      def eqv(x: CNil, y: CNil): Boolean = true
    }

  implicit def eqCCons[A, C <: Coproduct](implicit eqa: Eq[A], eqc: Eq[C]): Eq[A :+: C] =
    new Eq[A :+: C] {
      def eqv(x: A :+: C, y: A :+: C): Boolean =
        (x, y) match {
          case (Inl(xa), Inl(ya)) => eqa.eqv(xa, ya)
          case (Inr(xc), Inr(yc)) => eqc.eqv(xc, yc)
          case _ => false
        }
    }

  implicit def eqFn1[A, B]: Eq[A => B] =
    new Eq[A => B] {
      def eqv(x: A => B, y: A => B): Boolean = x == y //FIXME
    }

  implicit val eqCountable0: Eq[Types.IsCountable0] = Eq.fromUniversalEquals
  implicit val eqCountable1: Eq[Types.IsCountable1] = Eq.fromUniversalEquals
  implicit val eqCountable256: Eq[Types.IsCountable256] = Eq.fromUniversalEquals
  implicit val eqCountableZ: Eq[Types.IsCountableZ] = Eq.fromUniversalEquals

  implicit val eqIndexable0: Eq[Types.IsIndexable0] = Eq.fromUniversalEquals
  implicit val eqIndexable1: Eq[Types.IsIndexable1] = Eq.fromUniversalEquals
  implicit val eqIndexable256: Eq[Types.IsIndexable256] = Eq.fromUniversalEquals
  implicit val eqIndexableZ: Eq[Types.IsIndexableZ] = Eq.fromUniversalEquals
}

import EqInstances._

object CNothingLaws extends CountableTests[Nothing]
object CUnitLaws extends StrongIndexableTests[Unit]
object CBooleanLaws extends StrongIndexableTests[Boolean]
object CByteLaws extends StrongIndexableTests[Byte]
object CShortLaws extends StrongIndexableTests[Short]
object CCharLaws extends StrongIndexableTests[Char]
object CIntLaws extends StrongIndexableTests[Int]
object CLongLaws extends StrongIndexableTests[Long]
object CFloatLaws extends StrongIndexableTests[Float]
object CDoubleLaws extends StrongIndexableTests[Double]
object CZLaws extends StrongIndexableTests[Z]
object CStringLaws extends StrongIndexableTests[String]

object COptionUnitLaws extends StrongIndexableTests[Option[Unit]]
object COptionBooleanLaws extends StrongIndexableTests[Option[Boolean]]
object COptionIntLaws extends StrongIndexableTests[Option[Int]]
object CEitherUnitUnit extends StrongIndexableTests[Either[Unit, Unit]]
object CEitherIntByte extends StrongIndexableTests[Either[Int, Byte]]
object CTupleBooleanBoolean extends WeakIndexableTests[(Boolean, Boolean)]
object CTupleLongIntShortByte extends WeakIndexableTests[(Long, Int, Short, Byte)]
object CHListBBBB extends WeakIndexableTests[Byte :: Byte :: Byte :: Byte :: HNil]

object COptionStringLaws extends StrongIndexableTests[Option[String]]
object CEitherByteZ extends StrongIndexableTests[Either[Byte, Z]]
object CEitherZByte extends StrongIndexableTests[Either[Z, Byte]]
object CEitherZZ extends StrongIndexableTests[Either[Z, Z]]
object CSetBoolean extends StrongIndexableTests[Set[Boolean]]
object CSetByte extends StrongIndexableTests[Set[Byte]]
//object CSetShort extends StrongIndexableTests[Set[Short]] // passes, but is slow
object CSetShort extends WeakIndexableTests[Set[Short]]
object CSetInt extends WeakIndexableTests[Set[Int]]
object CSetZ extends WeakIndexableTests[Set[Z]]
object CListBoolean extends StrongIndexableTests[List[Boolean]]
object CListByte extends StrongIndexableTests[List[Byte]]
object CListInt extends StrongIndexableTests[List[Int]]
object CListZ extends StrongIndexableTests[List[Z]]
object CTupleZZZ extends WeakIndexableTests[(Z, Z, Z)]
object CTupleZByteByte extends WeakIndexableTests[(Z, Byte, Byte)]

object CMapBB extends CountableTests[Map[Byte, Byte]]
object CMapBS extends CountableTests[Map[Byte, String]]
object CMapSB extends CountableTests[Map[String, Byte]]
object CMapSS extends CountableTests[Map[String, String]]

object CFunction22 extends CountableTests[Boolean => Boolean]
object CFunctionBB extends CountableTests[Byte => Byte]
object CFunctionZB extends CountableTests[Z => Byte]

object Types {

  sealed abstract class CCompanion[A](c: Countable[A]) {
    implicit val instance: Countable[A] = c
  }

  sealed abstract class ICompanion[A](i: Indexable[A]) {
    implicit val instance: Indexable[A] = i
  }

  sealed trait IsCountable0
  case class IsCountable1()
  case class IsCountable256(value: Byte)
  case class IsCountableZ(value: Z)

  object IsCountable0 extends CCompanion(Countable.empty[IsCountable0])
  object Countable1 extends CCompanion(Countable.singleton(IsCountable1()))
  object Countable256 extends CCompanion(Countable[Byte].translate(IsCountable256(_)))
  object CountableZ extends CCompanion(Countable[Z].translate(IsCountableZ(_)))

  sealed trait IsIndexable0
  case class IsIndexable1()
  case class IsIndexable256(value: Byte)
  case class IsIndexableZ(value: Z)

  object IsIndexable0 extends ICompanion(Countable.empty[IsIndexable0])
  object Indexable1 extends ICompanion(Countable.singleton(IsIndexable1()))
  object Indexable256 extends ICompanion(Indexable[Byte].imap(IsIndexable256(_))(_.value))
  object IndexableZ extends ICompanion(Indexable[Z].imap(IsIndexableZ(_))(_.value))
}

import Types._

// generic countable

object IsCountable0 extends CountableTests[IsCountable0]
object OptionIsCountable0 extends CountableTests[Option[IsCountable0]]
object ListIsCountable0 extends CountableTests[List[IsCountable0]]
object SetIsCountable0 extends CountableTests[Set[IsCountable0]]
object VectorIsCountable0 extends CountableTests[Vector[IsCountable0]]
object MapKeyIsCountable0 extends CountableTests[Map[IsCountable0, Z]]
object MapValfIsCountable0 extends CountableTests[Map[Z, IsCountable0]]
object EitherIsCountable0 extends CountableTests[Either[Unit, IsCountable0]]
object PairIsCountable0 extends CountableTests[(IsCountable0, IsCountable0)]

object IsCountable1 extends CountableTests[IsCountable1]
object OptionIsCountable1 extends CountableTests[Option[IsCountable1]]
// object ListIsCountable1 extends CountableTests[List[IsCountable1]] // slow for large indices
object SetIsCountable1 extends CountableTests[Set[IsCountable1]]
// object VectorIsCountable1 extends CountableTests[Vector[IsCountable1]] // slow for large indices
object MapKeyIsCountable1 extends CountableTests[Map[IsCountable1, Z]]
object MapValfIsCountable1 extends CountableTests[Map[Z, IsCountable1]]
object EitherIsCountable1 extends CountableTests[Either[Unit, IsCountable1]]
object PairIsCountable1 extends CountableTests[(IsCountable1, IsCountable1)]

object IsCountable256 extends CountableTests[IsCountable256]
object OptionIsCountable256 extends CountableTests[Option[IsCountable256]]
object ListIsCountable256 extends CountableTests[List[IsCountable256]]
object SetIsCountable256 extends CountableTests[Set[IsCountable256]]
object VectorIsCountable256 extends CountableTests[Vector[IsCountable256]]
object MapKeyIsCountable256 extends CountableTests[Map[IsCountable256, Z]]
object MapValfIsCountable256 extends CountableTests[Map[Z, IsCountable256]]
object EitherIsCountable256 extends CountableTests[Either[Unit, IsCountable256]]
object PairIsCountable256 extends CountableTests[(IsCountable256, IsCountable256)]

object IsCountableZ extends CountableTests[IsCountableZ]
object OptionIsCountableZ extends CountableTests[Option[IsCountableZ]]
object ListIsCountableZ extends CountableTests[List[IsCountableZ]]
object SetIsCountableZ extends CountableTests[Set[IsCountableZ]]
object VectorIsCountableZ extends CountableTests[Vector[IsCountableZ]]
object MapKeyIsCountableZ extends CountableTests[Map[IsCountableZ, Z]]
object MapValfIsCountableZ extends CountableTests[Map[Z, IsCountableZ]]
object EitherIsCountableZ extends CountableTests[Either[Unit, IsCountableZ]]
object PairIsCountableZ extends CountableTests[(IsCountableZ, IsCountableZ)]

object CCoproduct0 extends CountableTests[CNil]
object CCoproduct1 extends CountableTests[IsCountable256 :+: CNil]
object CCoproduct2 extends CountableTests[IsCountableZ :+: CNil]
object CCoproduct3 extends CountableTests[IsCountable256 :+: IsCountableZ :+: CNil]
object CCoproduct4 extends CountableTests[IsCountableZ :+: IsCountable256 :+: CNil]

object CHList0 extends CountableTests[HNil]
object CHList1 extends CountableTests[IsCountable256 :: HNil]
object CHList2 extends CountableTests[IsCountableZ :: HNil]
object CHList3 extends CountableTests[IsCountable256 :: IsCountableZ :: HNil]
object CHList4 extends CountableTests[IsCountableZ :: IsCountable256 :: HNil]

// generic indexable

object IsIndexable0 extends WeakIndexableTests[IsIndexable0]
object OptionIsIndexable0 extends WeakIndexableTests[Option[IsIndexable0]]
object ListIsIndexable0 extends WeakIndexableTests[List[IsIndexable0]]
object SetIsIndexable0 extends WeakIndexableTests[Set[IsIndexable0]]
object VectorIsIndexable0 extends WeakIndexableTests[Vector[IsIndexable0]]
// object MapKeyIsIndexable0 extends WeakIndexableTests[Map[IsIndexable0, Z]]
// object MapValfIsIndexable0 extends WeakIndexableTests[Map[Z, IsIndexable0]]
object EitherIsIndexable0 extends WeakIndexableTests[Either[Unit, IsIndexable0]]
object PairIsIndexable0 extends WeakIndexableTests[(IsIndexable0, IsIndexable0)]

object IsIndexable1 extends WeakIndexableTests[IsIndexable1]
object OptionIsIndexable1 extends WeakIndexableTests[Option[IsIndexable1]]
// object ListIsIndexable1 extends WeakIndexableTests[List[IsIndexable1]] // slow for large indices
object SetIsIndexable1 extends WeakIndexableTests[Set[IsIndexable1]]
// object VectorIsIndexable1 extends WeakIndexableTests[Vector[IsIndexable1]] // slow for large indices
// object MapKeyIsIndexable1 extends WeakIndexableTests[Map[IsIndexable1, Z]]
// object MapValfIsIndexable1 extends WeakIndexableTests[Map[Z, IsIndexable1]]
object EitherIsIndexable1 extends WeakIndexableTests[Either[Unit, IsIndexable1]]
object PairIsIndexable1 extends WeakIndexableTests[(IsIndexable1, IsIndexable1)]

object IsIndexable256 extends WeakIndexableTests[IsIndexable256]
object OptionIsIndexable256 extends WeakIndexableTests[Option[IsIndexable256]]
object ListIsIndexable256 extends WeakIndexableTests[List[IsIndexable256]]
object SetIsIndexable256 extends WeakIndexableTests[Set[IsIndexable256]]
object VectorIsIndexable256 extends WeakIndexableTests[Vector[IsIndexable256]]
// object MapKeyIsIndexable256 extends WeakIndexableTests[Map[IsIndexable256, Z]]
// object MapValfIsIndexable256 extends WeakIndexableTests[Map[Z, IsIndexable256]]
object EitherIsIndexable256 extends WeakIndexableTests[Either[Unit, IsIndexable256]]
object PairIsIndexable256 extends WeakIndexableTests[(IsIndexable256, IsIndexable256)]

object IsIndexableZ extends WeakIndexableTests[IsIndexableZ]
object OptionIsIndexableZ extends WeakIndexableTests[Option[IsIndexableZ]]
object ListIsIndexableZ extends WeakIndexableTests[List[IsIndexableZ]]
object SetIsIndexableZ extends WeakIndexableTests[Set[IsIndexableZ]]
object VectorIsIndexableZ extends WeakIndexableTests[Vector[IsIndexableZ]]
// object MapKeyIsIndexableZ extends WeakIndexableTests[Map[IsIndexableZ, Z]]
// object MapValfIsIndexableZ extends WeakIndexableTests[Map[Z, IsIndexableZ]]
object EitherIsIndexableZ extends WeakIndexableTests[Either[Unit, IsIndexableZ]]
object PairIsIndexableZ extends WeakIndexableTests[(IsIndexableZ, IsIndexableZ)]

object ICoproduct0 extends WeakIndexableTests[CNil]
object ICoproduct1 extends WeakIndexableTests[IsIndexable256 :+: CNil]
object ICoproduct2 extends WeakIndexableTests[IsIndexableZ :+: CNil]
object ICoproduct3 extends WeakIndexableTests[IsIndexable256 :+: IsIndexableZ :+: CNil]
object ICoproduct4 extends WeakIndexableTests[IsIndexableZ :+: IsIndexable256 :+: CNil]

object IHList0 extends WeakIndexableTests[HNil]
object IHList1 extends WeakIndexableTests[IsIndexable256 :: HNil]
object IHList2 extends WeakIndexableTests[IsIndexableZ :: HNil]
object IHList3 extends WeakIndexableTests[IsIndexable256 :: IsIndexableZ :: HNil]
object IHList4 extends WeakIndexableTests[IsIndexableZ :: IsIndexable256 :: HNil]
