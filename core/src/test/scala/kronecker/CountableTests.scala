package kronecker

import org.scalacheck.{Arbitrary, Prop, Properties}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.{forAll, BooleanOperators}
import scala.reflect.runtime.universe.TypeTag
import shapeless._

import Testing._

object FixedCountableTests extends Properties("FixedCountableTests") {

  property("oneOf(c) = c") = {
    val c0 = Countable[Z]
    val c1 = Countable.oneOf(c0)
    forAll { (index: Z) =>
      c1.get(index) == c0.get(index)
    }
  }

  property("c.drop(k).get(i) = c.get(i + k)") =
    forAll { (k: Byte, indices: Set[Z]) =>
      val c0 = Countable[Z]
      val n = k & 0xff
      val c1 = c0.drop(n)
      indices.forall(i => c1.get(i) == c0.get(i + n))
    }

  property("iterator") = {
    val c = Countable[Z]
    c.iterator
      .take(1000)
      .zipWithIndex
      .forall { case (n, i) => c.get(i) == Some(n) }
  }

  property("iterator ~ toStream") =
    forAll { (k: Short) =>
      val c = Countable[Z]
      val n = k & 0xffff
      c.iterator.take(n).toList == c.stream.take(n).toList
    }
}
