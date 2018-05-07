package kronecker

import org.scalacheck.Properties
import shapeless._

object GenericTests extends Properties("Generic") {

  sealed trait Qux
  object Qux {
    case object LQ extends Qux
    case object RQ extends Qux
  }

  sealed trait IEither[A, B]
  object IEither {
    case class ILeft[A, B](a: A) extends IEither[A, B]
    case class IRight[A, B](b: B) extends IEither[A, B]
    case class IBoth[A, B](a: A, b: B) extends IEither[A, B]
  }

  case class Pair[A](first: A, second: A)

  // Countable[Qux]
  // Countable[Set[Qux]]
  // Countable[List[Qux]]
  // Countable[Either[Qux, Qux]]
  // Countable[Pair[Z]]
  // Countable[Pair[Qux]]
  // Countable[Pair[Set[Qux]]]
}
