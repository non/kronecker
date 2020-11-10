package kronecker

import cats.kernel.Eq
import spire.math.Interval
import spire.math.extras.interval.IntervalSeq

import kronecker.Intervals.{CountableInterval, CountableIntervalSeq}

object IntervalTests {
  case class A(value: Z)
  case class B(value: Z)
  case class C(value: Z)
  case class D(value: Z)
  case class E(value: Z)
  case class F(value: Z)
  case class G(value: Z)
  case class H(value: Z)
  case class I(value: Z)
  case class J(value: Z)
  case class K(value: Z)
  case class L(value: Z)

  lazy val iva = Interval.closed(Z(-100), Z(100))
  lazy val ivb = Interval.open(Z(300), Z(400))
  lazy val ivc = Interval.openLower(Z(-500), Z(-400))
  lazy val ivd = Interval.below(Z(-1000))
  lazy val ive = Interval.above(Z(1000))
  lazy val ivf = Interval.all[Z]
  lazy val isg = IntervalSeq(iva) | IntervalSeq(ivb) | IntervalSeq(ivc)
  lazy val ish = isg | IntervalSeq(ivd)
  lazy val isi = ish | IntervalSeq(ive)
  lazy val isj = IntervalSeq.empty[Z]

  implicit lazy val ca: Countable[A] = CountableInterval(iva).translate(A(_))
  implicit lazy val cb: Countable[B] = CountableInterval(ivb).translate(B(_))
  implicit lazy val cc: Countable[C] = CountableInterval(ivc).translate(C(_))
  implicit lazy val cd: Countable[D] = CountableInterval(ivd).translate(D(_))
  implicit lazy val ce: Countable[E] = CountableInterval(ive).translate(E(_))
  implicit lazy val cf: Countable[F] = CountableInterval(ivf).translate(F(_))
  implicit lazy val cg: Countable[G] = CountableIntervalSeq(isg).translate(G(_))
  implicit lazy val ch: Countable[H] = CountableIntervalSeq(ish).translate(H(_))
  implicit lazy val ci: Countable[I] = CountableIntervalSeq(isi).translate(I(_))
  implicit lazy val cj: Countable[J] = CountableIntervalSeq(isj).translate(J(_))

  implicit def univEq[T]: Eq[T] =
    Eq.fromUniversalEquals[T]
}

import IntervalTests._

object CountableIntervalA extends CountableTests[A]
object CountableIntervalB extends CountableTests[B]
object CountableIntervalC extends CountableTests[C]
object CountableIntervalD extends CountableTests[D]
object CountableIntervalE extends CountableTests[E]
//object CountableIntervalF extends CountableTests[F]
object CountableIntervalG extends CountableTests[G]
object CountableIntervalH extends CountableTests[H]
object CountableIntervalI extends CountableTests[I]
object CountableIntervalJ extends CountableTests[J]
