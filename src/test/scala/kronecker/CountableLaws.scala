package kronecker

import shapeless._

object CNothingLaws extends FiniteLaws[Nothing]
object CUnitLaws extends FiniteLaws[Unit]
object CBooleanLaws extends FiniteLaws[Boolean]
object CByteLaws extends FiniteLaws[Byte]
object CShortLaws extends FiniteLaws[Short]
object CCharLaws extends FiniteLaws[Char]
object CIntLaws extends FiniteLaws[Int]
object CLongLaws extends FiniteLaws[Long]
// object CFloatLaws extends FiniteLaws[Float] // NaN != NaN
// object CDoubleLaws extends FiniteLaws[Double] // NaN != NaN
object CZLaws extends InfiniteLaws[Z]
object CStringLaws extends InfiniteLaws[String]

object FOptionUnitLaws extends FiniteLaws[Option[Unit]]
object FOptionBooleanLaws extends FiniteLaws[Option[Boolean]]
object FOptionIntLaws extends FiniteLaws[Option[Int]]
object FEitherUnitUnit extends FiniteLaws[Either[Unit, Unit]]
object FEitherIntByte extends FiniteLaws[Either[Int, Byte]]
object FSetByte extends FiniteLaws[Set[Byte]]
object FTupleBooleanBoolean extends FiniteLaws[(Boolean, Boolean)]
object FTupleLongIntShortByte extends FiniteLaws[(Long, Int, Short, Byte)]
object FHListBBBB extends FiniteLaws[Byte :: Byte :: Byte :: Byte :: HNil]

object IOptionStringLaws extends InfiniteLaws[Option[String]]
object IEitherByteZ extends InfiniteLaws[Either[Byte, Z]]
object IEitherZByte extends InfiniteLaws[Either[Z, Byte]]
object IEitherZZ extends InfiniteLaws[Either[Z, Z]]
object ISetZ extends InfiniteLaws[Set[Z]]
object IListByte extends InfiniteLaws[List[Byte]]
object IListInt extends InfiniteLaws[List[Int]]
object ITupleZZZ extends InfiniteLaws[(Z, Z, Z)]
object ITupleZByteByte extends InfiniteLaws[(Z, Byte, Byte)]
