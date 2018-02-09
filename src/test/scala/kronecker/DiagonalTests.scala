package kronecker

import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.forAll
import scala.annotation.tailrec

object DiagonalTests extends Properties("Diagonal") {

  val tiny: Gen[Int] = Gen.choose(1, 5)
  val small: Gen[Int] = Gen.choose(1, 10)
  val medium: Gen[Int] = Gen.choose(1, 100)

  val prettyBig: Gen[Z] = Gen.choose(0, Int.MaxValue).map(Z(_))
  val big: Gen[Z] = Gen.choose(0, Long.MaxValue).map(Z(_))
  val veryBig: Gen[Z] = for { x <- big; y <- big } yield x * y

  property("widthAtDepth") =
    forAll(small, medium) { (dim: Int, depth0: Int) =>
      val w0 = Diagonal.widthAtDepth(dim, depth0)
      val w1 = Diagonal.widthAtDepth(dim, depth0 + 1)
      val w2 = Diagonal.widthAtDepth(dim, depth0 + 2)

      if (dim == 1) {
        (w0 == 1) && (w1 == w0) && (w2 == w1)
      } else {
        (w0 > 0) && (w1 > w0) && (w2 > w1)
      }
    }

  property("decompose") =
    forAll(small, medium) { (dim: Int, index: Int) =>
      val got = Diagonal.decompose(dim, Z(index))
      val expected = oldDecompose(dim, Z(index))
      if (got != expected) println((got, expected))
      got == expected
    }

  case class Info(nextTerm: Z, num: Z, denom: Z)

  def infoAtDepth(dim: Int, depth: Z): Info = {
    @tailrec def loop(i: Int, term: Z, num: Z, denom: Z): Info =
      if (i <= 1) Info(term, num, denom)
       else loop(i - 1, term + 1, (depth + term) * num, denom * term)
    loop(dim, Z.one, Z.one, Z.one)
  }

  /**
   * Decompose an index into a position and depth.
   */
  def oldDecompose(dim: Int, index: Z): (Z, Z) =
    if (dim == 1) {
      (Z.zero, index)
    } else if (dim == 2) {
      var i = index
      var depth = Z.zero
      var num = Z.one
      while (i >= num) {
        i -= num
        depth += 1
        num += 1
      }
      (i, depth)
    } else {
      val info = infoAtDepth(dim, 0)
      var i = index * info.denom
      var depth = Z.zero
      var num = info.num
      var oldestTerm = Z.one
      var nextTerm = info.nextTerm
      while (i >= num) {
        i -= num
        depth += 1
        num = (num * nextTerm) / oldestTerm
        oldestTerm += 1
        nextTerm += 1
      }

      (i / info.denom, depth)
    }
  
}
