package kronecker

import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.forAll

import Testing._
import Card.{zero, one}

class CardLaws extends Properties("Card") {

  property("x + 0 = 0 + x = x") =
    forAll { (x: Card) =>
      ((x + zero) == x) && ((zero + x) == x)
    }

  property("(x + y) + z = x + (y + z)") =
    forAll { (x: Card, y: Card, z: Card) =>
      ((x + y) + z) == (x + (y + z))
    }

  property("x * 0 = 0 * x = 0") =
    forAll { (x: Card) =>
      ((x * zero) == zero) && ((zero * x) == zero)
    }

  property("x * 1 = 1 * x = x") =
    forAll { (x: Card) =>
      ((x * one) == x) && ((one * x) == x)
    }

  property("(x * y) * z = x * (y * z)") =
    forAll { (x: Card, y: Card, z: Card) =>
      ((x * y) * z) == (x * (y * z))
    }

  // NOTE: not currently testing distribution since we'll need to add
  // a bunch of cases to get it right for the free construction.

  property("(x ^ y) ^ z = x ^ (y * z)") =
    forAll { (x: Card, y: Card, z: Card) =>
      ((x ** y) ** z) == (x ** (y * z))
    }
}
