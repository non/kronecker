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

trait IndexableLaws[A] extends CountableLaws[A] { self: Properties =>

  def ev: Indexable[A]
  implicit def arb: Arbitrary[A]

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
object CStringLaws extends IndexableTests[String]

object FOptionUnitLaws extends IndexableTests[Option[Unit]]
object FOptionBooleanLaws extends IndexableTests[Option[Boolean]]
object FOptionIntLaws extends IndexableTests[Option[Int]]
object FEitherUnitUnit extends IndexableTests[Either[Unit, Unit]]
object FEitherIntByte extends IndexableTests[Either[Int, Byte]]
object FSetByte extends IndexableTests[Set[Byte]]
object FTupleBooleanBoolean extends CountableTests[(Boolean, Boolean)]
object FTupleLongIntShortByte extends CountableTests[(Long, Int, Short, Byte)]
object FHListBBBB extends CountableTests[Byte :: Byte :: Byte :: Byte :: HNil]

object IOptionStringLaws extends IndexableTests[Option[String]]
object IEitherByteZ extends IndexableTests[Either[Byte, Z]]
object IEitherZByte extends IndexableTests[Either[Z, Byte]]
object IEitherZZ extends IndexableTests[Either[Z, Z]]
object ISetBoolean extends CountableTests[Set[Boolean]]
object ISetByte extends CountableTests[Set[Byte]]
object ISetInt extends CountableTests[Set[Int]]
object ISetZ extends CountableTests[Set[Z]]
object IListBoolean extends IndexableTests[List[Boolean]]
object IListByte extends IndexableTests[List[Byte]]
object IListInt extends IndexableTests[List[Int]]
object IListZ extends IndexableTests[List[Z]]
object ITupleZZZ extends CountableTests[(Z, Z, Z)]
object ITupleZByteByte extends CountableTests[(Z, Byte, Byte)]

object FFMapBB extends CountableTests[Map[Byte, Byte]]
object FIMapBS extends CountableTests[Map[Byte, String]]
object IFMapSB extends CountableTests[Map[String, Byte]]
object IIMapSS extends CountableTests[Map[String, String]]

object CFunction22 extends CountableTests[Boolean => Boolean]
object CFunctionBB extends CountableTests[Byte => Byte]
object CFunctionZB extends CountableTests[Z => Byte]
