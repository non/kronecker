package kronecker
package refined

import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.{boolean => b}
import eu.timepit.refined.{generic => g}
import eu.timepit.refined.{numeric => n}
import eu.timepit.refined.internal.{WitnessAs => W}

import shapeless.{Witness => L}

import spire.algebra.Order
import spire.math.{Interval, Searching}
import spire.math.extras.interval.IntervalSeq
import spire.implicits._

import scala.collection.mutable

trait AsZ[A] {
  type Image <: Z
  def to(a: A): Image
  def from(z: Image): A
  def toz(a: A): Z
  def fromz(z: Z): Option[A]
}

object AsZ {
  implicit val byteAsZ: AsZ[Byte] =
    new AsZ[Byte] {
      type Image = Z
      def to(a: Byte): Image = Z(a)
      def from(z: Image): Byte = z.toByte
      def toz(a: Byte): Z = Z(a)
      def fromz(z: Z): Option[Byte] = if (z.isValidByte) Some(z.toByte) else None
    }

  implicit val shortAsZ: AsZ[Short] =
    new AsZ[Short] {
      type Image = Z
      def to(a: Short): Image = Z(a)
      def from(z: Image): Short = z.toShort
      def toz(a: Short): Z = Z(a)
      def fromz(z: Z): Option[Short] = if (z.isValidShort) Some(z.toShort) else None
    }

  implicit val charAsZ: AsZ[Char] =
    new AsZ[Char] {
      type Image = Z
      def to(a: Char): Image = Z(a)
      def from(z: Image): Char = z.toChar
      def toz(a: Char): Z = Z(a)
      def fromz(z: Z): Option[Char] = if (z.isValidChar) Some(z.toChar) else None
    }

  implicit val intAsZ: AsZ[Int] =
    new AsZ[Int] {
      type Image = Z
      def to(a: Int): Image = Z(a)
      def from(z: Image): Int = z.toInt
      def toz(a: Int): Z = Z(a)
      def fromz(z: Z): Option[Int] = if (z.isValidInt) Some(z.toInt) else None
    }

  implicit val longAsZ: AsZ[Long] =
    new AsZ[Long] {
      type Image = Z
      def to(a: Long): Image = Z(a)
      def from(z: Image): Long = z.toLong
      def toz(a: Long): Z = Z(a)
      def fromz(z: Z): Option[Long] = if (z.isValidLong) Some(z.toLong) else None
    }

  implicit val bigIntAsZ: AsZ[BigInt] =
    new AsZ[BigInt] {
      type Image = Z
      def to(a: BigInt): Image = Z(a)
      def from(z: Image): BigInt = z.toBigInt
      def toz(a: BigInt): Z = Z(a)
      def fromz(z: Z): Option[BigInt] = Some(z.toBigInt)
    }

  implicit val safeLongAsZ: AsZ[Z] =
    new AsZ[Z] {
      type Image = Z
      def to(a: Z): Image = a
      def from(z: Image): Z = z
      def toz(a: Z): Z = a
      def fromz(z: Z): Option[Z] = Some(z)
    }
}

sealed trait Refinement[T, P] {
  def toIntervalSeq(implicit ev: Order[T]): IntervalSeq[T] = {
    def recurse(r: Refinement[T, _]): IntervalSeq[T] =
      r match {
        case Refinement.Equal(wn) => IntervalSeq.point(wn.snd)
        case Refinement.Less(wn) => IntervalSeq.below(wn.snd)
        case Refinement.Greater(wn) => IntervalSeq.above(wn.snd)
        case Refinement.Not(r) => ~recurse(r)
        case Refinement.And(rp, rq) => recurse(rq) & recurse(rp)
        case Refinement.Or(rp, rq) => recurse(rq) | recurse(rp)
      }
    recurse(this)
  }

  def toCountable(implicit ev: Order[T], asz: AsZ[T]): Countable[Refined[T, P]] = {
    implicit val o: Order[asz.Image] = ???
    //val isq: IntervalSeq[Z] =
    val isq: IntervalSeq[asz.Image] =
      toIntervalSeq(ev)
        .intervalIterator
        //.map(iv => IntervalSeq(iv.mapBounds(asz.toz)))
        .map(iv => IntervalSeq(iv.mapBounds(asz.to(_))))
        .reduceLeft(_ | _)
    // Intervals.CountableIntervalSeq(isq)
    //   .translate(asz.fromz(_).get.asInstanceOf[Refined[T, P]])
    ???
  }
}

object Refinement {

  implicit def apply[T, P](implicit ev: Refinement[T, P]): Refinement[T, P] = ev

  case class Equal[T, N](wn: W[N, T]) extends Refinement[T, g.Equal[N]]
  case class Less[T, N](wn: W[N, T]) extends Refinement[T, n.Less[N]]
  case class Greater[T, N](wn: W[N, T]) extends Refinement[T, n.Greater[N]]
  case class Not[T, P](r: Refinement[T, P]) extends Refinement[T, b.Not[P]]
  case class And[T, P, Q](rp: Refinement[T, P], rq: Refinement[T, Q]) extends Refinement[T, b.And[P, Q]]
  case class Or[T, P, Q](rp: Refinement[T, P], rq: Refinement[T, Q]) extends Refinement[T, b.Or[P, Q]]

  implicit def forLess[T, N](implicit wn: W[N, T]): Refinement[T, n.Less[N]] =
    Less(wn)

  implicit def forGreater[T, N](implicit wn: W[N, T]): Refinement[T, n.Greater[N]] =
    Greater(wn)

  implicit def forNot[T, P](implicit r: Refinement[T, P]): Refinement[T, b.Not[P]] =
    Not(r)

  implicit def forAnd[T, P, Q](implicit rp: Refinement[T, P], rq: Refinement[T, Q]): Refinement[T, b.And[P, Q]] =
    And(rp, rq)

  implicit def forOr[T, P, Q](implicit rp: Refinement[T, P], rq: Refinement[T, Q]): Refinement[T, b.Or[P, Q]] =
    Or(rp, rq)


  type G0 = n.Greater[L.`0`.T]
  type G50 = n.Greater[L.`0`.T]
  type G300 = n.Greater[L.`0`.T]
  type L100 = n.Less[L.`100`.T]
  type L150 = n.Less[L.`150`.T]
  type L350 = n.Less[L.`350`.T]

  type P1 = b.And[G0, L100]
  type P2 = b.And[G50, L150]
  type P3 = b.And[G300, L350]

  type X = b.Or[b.Or[P1, P2], P3]
  type Y = b.Or[b.Or[P1, P2], P3]

  val cx = Refinement[Int, X].toCountable
  val cy = Refinement[Int, Y].toCountable

  val x: Refined[Int, X] = 16
  val y: Refined[Int, Y] = 83
}
