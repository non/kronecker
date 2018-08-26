package kronecker

import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.forAll
import scala.annotation.tailrec

object PermutationTests extends Properties("Diagonal") {

  // size of the set we will be permutating
  val n = 100

  val num: Gen[Int] = Gen.choose(1, n)
  val pair: Gen[(Int, Int)] = Gen.zip(num, num)
  val pairs: Gen[List[(Int, Int)]] = Gen.listOf(pair)

  val p0: Permutation[Int] = Permutation.identity[Int]

  val perm: Gen[Permutation[Int]] =
    pairs.map(_.foldLeft(p0) { case (p, (x, y)) => p.swap(x, y) })

  def isValid(p: Permutation[Int]): Boolean =
    (1 to n).map(p(_)).toSet.size == n

  property("valid permutation") =
    forAll(perm)(isValid)

  property("(x = y) = (p(x) = p(y))") =
    forAll(perm, num, num) { (p, x, y) =>
      (x == y) == (p(x) == p(y))
    }

  property("p.swap(x, y) = p andThen Permutation(x, y)") =
    forAll(perm, num, num) { (p, x, y) =>
      p.swap(x, y) == (p andThen Permutation(x, y))
    }

  property("p0 andThen p1 is valid") =
    forAll(perm, perm) { (p0, p1) =>
      isValid(p0 andThen p1)
    }

  property("p.reverse.reverse = p") =
    forAll(perm) { (p) =>
      p.reverse.reverse == p
    }

  property("(p0 andThen p1).reverse = (p1.reverse andThen p0.reverse)") =
    forAll(perm, perm) { (p0, p1) =>
      val x = (p0 andThen p1).reverse
      val y = p1.reverse andThen p0.reverse
      x == y
    }

  property("(p0 andThen p1) = (p1 compose p0)") =
    forAll(perm, perm) { (p0, p1) =>
      (p0 andThen p1) == (p1 compose p0)
    }

  property("p andThen p.reverse = identity") =
    forAll(perm) { p =>
      val p1 = (p andThen p.reverse)
      if (p1 != p0) println((p1, p0))
      p1 == p0
    }

  property("(p0 = p1) = (forall x: p0(x) = p1(x))") =
    forAll(perm, perm) { (p0, p1) =>
      val a = p0 == p1
      val b = (1 to 100).forall(x => p0(x) == p1(x))
      a == b
    }

  property("p.swap(x, y).swap(x, y) = p") =
    forAll(perm, num, num) { (p, x, y) =>
      p.swap(x, y).swap(x, y) == p
    }
}
