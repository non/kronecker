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

object FOptionUnitLaws extends StrongIndexableTests[Option[Unit]]
object FOptionBooleanLaws extends StrongIndexableTests[Option[Boolean]]
object FOptionIntLaws extends StrongIndexableTests[Option[Int]]
object FEitherUnitUnit extends StrongIndexableTests[Either[Unit, Unit]]
object FEitherIntByte extends StrongIndexableTests[Either[Int, Byte]]
object FSetByte extends StrongIndexableTests[Set[Byte]]
object FTupleBooleanBoolean extends CountableTests[(Boolean, Boolean)]
object FTupleLongIntShortByte extends CountableTests[(Long, Int, Short, Byte)]
object FHListBBBB extends CountableTests[Byte :: Byte :: Byte :: Byte :: HNil]

object IOptionStringLaws extends StrongIndexableTests[Option[String]]
object IEitherByteZ extends StrongIndexableTests[Either[Byte, Z]]
object IEitherZByte extends StrongIndexableTests[Either[Z, Byte]]
object IEitherZZ extends StrongIndexableTests[Either[Z, Z]]
object ISetBoolean extends StrongIndexableTests[Set[Boolean]]
object ISetByte extends StrongIndexableTests[Set[Byte]]
object ISetInt extends WeakIndexableTests[Set[Int]]
object ISetZ extends WeakIndexableTests[Set[Z]]
object IListBoolean extends StrongIndexableTests[List[Boolean]]
object IListByte extends StrongIndexableTests[List[Byte]]
object IListInt extends StrongIndexableTests[List[Int]]
object IListZ extends StrongIndexableTests[List[Z]]
object ITupleZZZ extends CountableTests[(Z, Z, Z)]
object ITupleZByteByte extends CountableTests[(Z, Byte, Byte)]

object FFMapBB extends CountableTests[Map[Byte, Byte]]
object FIMapBS extends CountableTests[Map[Byte, String]]
object IFMapSB extends CountableTests[Map[String, Byte]]
object IIMapSS extends CountableTests[Map[String, String]]

object CFunction22 extends CountableTests[Boolean => Boolean]
object CFunctionBB extends CountableTests[Byte => Byte]
object CFunctionZB extends CountableTests[Z => Byte]
