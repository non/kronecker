package kronecker

import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import scala.reflect.runtime.universe.TypeTag
import shapeless._

import Testing._

abstract class CountableLaws[A](implicit c: Countable[A], tt: TypeTag[A])
    extends Properties(s"CountableLaws[${tt.tpe}]") {

  val card = c.cardinality

  property("get(i) is defined iff i < cardinality") =
    forAll { (i: Z) =>
      card.contains(i) == c.get(i).isDefined
    }

  property("get(i) = get(j) iff i = j") =
    forAll { (i1: Z, i2: Z) =>
      val (o1, o2) = (c.get(i1), c.get(i2))
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

object CNothingLaws extends CountableLaws[Nothing]
object CUnitLaws extends CountableLaws[Unit]
object CBooleanLaws extends CountableLaws[Boolean]
object CByteLaws extends CountableLaws[Byte]
object CShortLaws extends CountableLaws[Short]
object CCharLaws extends CountableLaws[Char]
object CIntLaws extends CountableLaws[Int]
object CLongLaws extends CountableLaws[Long]
// object CFloatLaws extends CountableLaws[Float] // NaN != NaN
// object CDoubleLaws extends CountableLaws[Double] // NaN != NaN
object CZLaws extends CountableLaws[Z]
object CStringLaws extends CountableLaws[String]

object FOptionUnitLaws extends CountableLaws[Option[Unit]]
object FOptionBooleanLaws extends CountableLaws[Option[Boolean]]
object FOptionIntLaws extends CountableLaws[Option[Int]]
object FEitherUnitUnit extends CountableLaws[Either[Unit, Unit]]
object FEitherIntByte extends CountableLaws[Either[Int, Byte]]
object FSetByte extends CountableLaws[Set[Byte]]
object FTupleBooleanBoolean extends CountableLaws[(Boolean, Boolean)]
object FTupleLongIntShortByte extends CountableLaws[(Long, Int, Short, Byte)]
object FHListBBBB extends CountableLaws[Byte :: Byte :: Byte :: Byte :: HNil]

object IOptionStringLaws extends CountableLaws[Option[String]]
object IEitherByteZ extends CountableLaws[Either[Byte, Z]]
object IEitherZByte extends CountableLaws[Either[Z, Byte]]
object IEitherZZ extends CountableLaws[Either[Z, Z]]
object ISetZ extends CountableLaws[Set[Z]]
object IListByte extends CountableLaws[List[Byte]]
object IListInt extends CountableLaws[List[Int]]
object ITupleZZZ extends CountableLaws[(Z, Z, Z)]
object ITupleZByteByte extends CountableLaws[(Z, Byte, Byte)]
