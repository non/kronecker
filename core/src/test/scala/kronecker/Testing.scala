package kronecker

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import spire.math.{Rational, SafeLong}

object Testing {

  val genZ: Gen[Z] =
    arbitrary[BigInt].map(n => Z(n.abs))

  val genFinite: Gen[Card] =
    genZ.map(Card(_))

  val genInfinite: Gen[Card] =
    Gen.const(Card.infinite)

  val freqs: List[(Int, Gen[Card])] =
    (10 -> genFinite) :: (5 -> genInfinite) :: Nil

  def genCard(n: Int): Gen[Card] =
    if (n > 0) {
      val g = genCard(n - 1)
      val g1 = for { x <- g; y <- g } yield x + y
      val g2 = for { x <- g; y <- g } yield x * y
      val g3 = for { x <- g; y <- g } yield x ** y
      val pairs = (n, g1) :: (n, g2) :: (n, g3) :: freqs
      Gen.frequency(pairs: _*)
    } else {
      Gen.frequency(freqs: _*)
    }

  implicit val arbitraryZ: Arbitrary[Z] =
    Arbitrary(genZ)

  implicit val arbitraryRatinoal: Arbitrary[Rational] = {
    val g = Gen.choose(1L, Long.MaxValue).map(SafeLong(_))
    Arbitrary(for {
      s <- Gen.frequency(15 -> 1L, 14 -> -1L, 1 -> 0L)
      nx <- g
      ny <- g
      dx <- g
      dy <- g
    } yield Rational(nx * ny, dx * dy) * s)
  }

  implicit val arbitraryCard: Arbitrary[Card] =
    Arbitrary(genCard(5))
}
