package kronecker

import org.scalacheck.{Arbitrary, Prop, Properties}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.{forAll, BooleanOperators}
import scala.reflect.runtime.universe.TypeTag
import shapeless._

import Testing._

trait CountableLaws[A] { self: Properties =>

  def ev: Countable[A]
  def tt: TypeTag[A]

  val card = ev.cardinality

  property("get(i) is defined iff i < cardinality") =
    forAll { (i: Z) =>
      card.contains(i) == ev.get(i).isDefined
    }

  property("get(i) = get(j) iff i = j") = {
    forAll { (i1: Z, i2: Z) =>
      val (o1, o2) = (ev.get(i1), ev.get(i2))
      val res = (card.contains(i1), card.contains(i2)) match {
        case (true, true) =>
          (o1 == o2) == (i1 == i2)
        case (true, false) =>
          o1.isDefined && o2.isEmpty
        case (false, true) =>
          o1.isEmpty && o2.isDefined
        case (false, false) =>
          o1 == None && o2 == None
      }
      if (!res) println(s"card=$card i1=$i1 i2=$i2 o1=$o1 o2=$o2 res=$res")
      res
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
    (ns.isEmpty: Boolean) :| s"$ns should be empty"
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
      val ok = o == Option(a)
      if (!ok) println(s"a=$a i=$i o=$o")
      ok
    }

  property("index(a1) = index(a2) iff a1 = a2") =
    forAll { (a1: A, a2: A) =>
      val i1 = ev.index(a1)
      val i2 = ev.index(a2)
      val ok = (i1 == i2) == (a1 == a2)
      if (!ok) println(s"a1=$a1, a2=$a2, i1=$i1, i2=$i2")
      ok
    }
}

abstract class CountableTests[A](implicit val ev: Countable[A], val tt: TypeTag[A])
    extends Properties(s"CountableTests[${tt.tpe}]") with CountableLaws[A]

abstract class WeakIndexableTests[A](implicit val ev: Indexable[A], val tt: TypeTag[A])
    extends Properties(s"WeakIndexableTests[${tt.tpe}]") with WeakIndexableLaws[A]

abstract class StrongIndexableTests[A](implicit val ev: Indexable[A], val arb: Arbitrary[A], val tt: TypeTag[A])
    extends Properties(s"StrongIndexableTests[${tt.tpe}]") with StrongIndexableLaws[A]

object CNothingLaws extends CountableTests[Nothing]
object CUnitLaws extends StrongIndexableTests[Unit]
object CBooleanLaws extends StrongIndexableTests[Boolean]
object CByteLaws extends StrongIndexableTests[Byte]
object CShortLaws extends StrongIndexableTests[Short]
object CCharLaws extends StrongIndexableTests[Char]
object CIntLaws extends StrongIndexableTests[Int]
object CLongLaws extends StrongIndexableTests[Long]
// object CFloatLaws extends StrongIndexableTests[Float] // NaN != NaN
// object CDoubleLaws extends StrongIndexableTests[Double] // NaN != NaN
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

object CEBB extends CountableTests[Either[IsCountable256, IsCountable256]]
object CEBZ extends CountableTests[Either[IsCountable256, IsCountableZ]]
object CEZB extends CountableTests[Either[IsCountableZ, IsCountable256]]
object CEZZ extends CountableTests[Either[IsCountableZ, IsCountableZ]]

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
