package kronecker

import org.scalacheck.{Arbitrary, Properties}
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

  property("get(i) = get(j) iff i = j") =
    forAll { (i1: Z, i2: Z) =>
      val (o1, o2) = (ev.get(i1), ev.get(i2))
      (card.contains(i1), card.contains(i2)) match {
        case (true, true) =>
          (o1 == o2) == (i1 == i2)
        case (true, false) =>
          o1.isDefined && o2.isEmpty
        case (false, true) =>
          o1.isEmpty && o2.isDefined
        case (false, false) =>
          o1 == None && o2 == None
      }
    }
}

trait IndexableLaws[A] extends CountableLaws[A] { self: Properties =>

  def ev: Indexable[A]
  implicit def arb: Arbitrary[A]

  property("sanity check") = {
    val ns = (0 until 20)
      .map { i => val o = ev.get(Z(i)); (i, o.map(a => (a, ev.index(a)))) }
      .flatMap { case (i, o) => o.map { case (a, n) => (i, a, n) } }
      .filter { case (i, a, n) => i != n }
      .map { case (i, a, n) => (i, a, n, ev.get(n)) }
      .toList
    (ns.isEmpty: Boolean) :| s"$ns should be empty"
  }

  property("0 <= index(a) < cardinality") =
    forAll { (a: A) =>
      card.contains(ev.index(a))
    }

  property("get(index(a)) = Some(a)") =
    forAll { (a: A) =>
      ev.get(ev.index(a)) == Some(a)
    }

  property("index(a1) = index(a2) iff a1 = a2") =
    forAll { (a1: A, a2: A) =>
      (ev.index(a1) == ev.index(a2)) == (a1 == a2)
    }
}

abstract class CountableTests[A](implicit val ev: Countable[A], val tt: TypeTag[A])
    extends Properties(s"CountableTests[${tt.tpe}]") with CountableLaws[A]

abstract class IndexableTests[A](implicit val ev: Indexable[A], val arb: Arbitrary[A], val tt: TypeTag[A])
    extends Properties(s"IndexableTests[${tt.tpe}]") with IndexableLaws[A]

object CNothingLaws extends CountableTests[Nothing]
object CUnitLaws extends IndexableTests[Unit]
object CBooleanLaws extends IndexableTests[Boolean]
object CByteLaws extends IndexableTests[Byte]
object CShortLaws extends IndexableTests[Short]
object CCharLaws extends IndexableTests[Char]
object CIntLaws extends IndexableTests[Int]
object CLongLaws extends IndexableTests[Long]
// object CFloatLaws extends IndexableTests[Float] // NaN != NaN
// object CDoubleLaws extends IndexableTests[Double] // NaN != NaN
object CZLaws extends IndexableTests[Z]
object CStringLaws extends CountableTests[String]

object FOptionUnitLaws extends IndexableTests[Option[Unit]]
object FOptionBooleanLaws extends IndexableTests[Option[Boolean]]
object FOptionIntLaws extends IndexableTests[Option[Int]]
object FEitherUnitUnit extends IndexableTests[Either[Unit, Unit]]
object FEitherIntByte extends IndexableTests[Either[Int, Byte]]
object FSetByte extends CountableTests[Set[Byte]]
object FTupleBooleanBoolean extends CountableTests[(Boolean, Boolean)]
object FTupleLongIntShortByte extends CountableTests[(Long, Int, Short, Byte)]
object FHListBBBB extends CountableTests[Byte :: Byte :: Byte :: Byte :: HNil]

object IOptionStringLaws extends CountableTests[Option[String]]
object IEitherByteZ extends IndexableTests[Either[Byte, Z]]
object IEitherZByte extends IndexableTests[Either[Z, Byte]]
object IEitherZZ extends IndexableTests[Either[Z, Z]]
object ISetZ extends CountableTests[Set[Z]]
object IListByte extends CountableTests[List[Byte]]
object IListInt extends CountableTests[List[Int]]
object IListZ extends IndexableTests[List[Z]]
object ITupleZZZ extends CountableTests[(Z, Z, Z)]
object ITupleZByteByte extends CountableTests[(Z, Byte, Byte)]
