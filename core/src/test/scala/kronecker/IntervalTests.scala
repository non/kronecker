package kronecker

import spire.implicits._
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

  val iva = Interval.closed(Z(-100), Z(100))
  val ivb = Interval.open(Z(300), Z(400))
  val ivc = Interval.openLower(Z(-500), Z(-400))
  val ivd = Interval.below(Z(-1000))
  val ive = Interval.above(Z(1000))
  val ivf = Interval.all[Z]
  val isg = IntervalSeq(iva) | IntervalSeq(ivb) | IntervalSeq(ivc)
  val ish = isg | IntervalSeq(ivd)
  val isi = ish | IntervalSeq(ive)
  val isj = IntervalSeq.empty[Z]

  implicit val ca: Countable[A] = CountableInterval(iva).translate(A(_))
  implicit val cb: Countable[B] = CountableInterval(ivb).translate(B(_))
  implicit val cc: Countable[C] = CountableInterval(ivc).translate(C(_))
  implicit val cd: Countable[D] = CountableInterval(ivd).translate(D(_))
  implicit val ce: Countable[E] = CountableInterval(ive).translate(E(_))
  implicit val cf: Countable[F] = CountableInterval(ivf).translate(F(_))
  implicit val cg: Countable[G] = CountableIntervalSeq(isg).translate(G(_))
  implicit val ch: Countable[H] = CountableIntervalSeq(ish).translate(H(_))
  implicit val ci: Countable[I] = CountableIntervalSeq(isi).translate(I(_))
  implicit val cj: Countable[J] = CountableIntervalSeq(isj).translate(J(_))
}

import IntervalTests._

object CountableIntervalA extends CountableTests[A]
object CountableIntervalB extends CountableTests[B]
object CountableIntervalC extends CountableTests[C]
object CountableIntervalD extends CountableTests[D]
object CountableIntervalE extends CountableTests[E]
object CountableIntervalF extends CountableTests[F]
object CountableIntervalG extends CountableTests[G]
object CountableIntervalH extends CountableTests[H]
object CountableIntervalI extends CountableTests[I]
object CountableIntervalJ extends CountableTests[J]
