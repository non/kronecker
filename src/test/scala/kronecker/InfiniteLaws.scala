package kronecker

import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import scala.reflect.runtime.universe.TypeTag

import Testing._

abstract class InfiniteLaws[A](implicit c: Countable.Infinite[A], tt: TypeTag[A])
    extends Properties(s"InfiniteLaws[${tt.tpe}]") {

  property("valid cardinality") =
    c.cardinality == Card.Infinite

  property("apply(i) is defined") =
    forAll { (i: Z) => c.get(i).isDefined }

  property("ev(i) = ev(j) iff i = j") =
    forAll { (i: Z, j: Z) =>
      (c(i) == c(j)) == (i == j)
    }
}
