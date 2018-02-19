package kronecker

import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import scala.reflect.runtime.universe.TypeTag

import Testing._

abstract class FiniteLaws[A](implicit c: Finite[A], tt: TypeTag[A])
    extends Properties(s"FiniteLaws[${tt.tpe}]") {

  val card = c.cardinality

  property("valid cardinality") =
    card match {
      case Card.Zero => c.size == 0
      case Card.Finite(n) => c.size == n
      case Card.Infinite => false
    }

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
