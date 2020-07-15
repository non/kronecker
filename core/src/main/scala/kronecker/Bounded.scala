package kronecker

import spire.math.SafeLong

trait Bounded[A] {
  def countable: Countable[A]
  def upper: Option[A]
  def lower: Option[A]
  def next(a: A): A
  def prev(a: A): A
  def toZ(a: A): Z
  def offset(first: A, distance: Z): A
}

object Bounded {

  implicit def boundedForByte: Bounded[Byte] =
    new Bounded[Byte] {
      def countable: Countable[Byte] = Countable[Byte]
      def upper: Option[Byte] = Some(Byte.MaxValue)
      def lower: Option[Byte] = Some(Byte.MinValue)
      def next(n: Byte): Byte = (n + 1).toByte
      def prev(n: Byte): Byte = (n - 1).toByte
      def toZ(n: Byte): Z = Z(n)
      def offset(first: Byte, distance: Z): Byte = (first + distance.toInt).toByte
    }

  implicit def boundedForShort: Bounded[Short] =
    new Bounded[Short] {
      def countable: Countable[Short] = Countable[Short]
      def upper: Option[Short] = Some(Short.MaxValue)
      def lower: Option[Short] = Some(Short.MinValue)
      def next(n: Short): Short = (n + 1).toShort
      def prev(n: Short): Short = (n - 1).toShort
      def toZ(n: Short): Z = Z(n)
      def offset(first: Short, distance: Z): Short = (first + distance.toInt).toShort
    }

  implicit def boundedForInt: Bounded[Int] =
    new Bounded[Int] {
      def countable: Countable[Int] = Countable[Int]
      def upper: Option[Int] = Some(Int.MaxValue)
      def lower: Option[Int] = Some(Int.MinValue)
      def next(n: Int): Int = n + 1
      def prev(n: Int): Int = n - 1
      def toZ(n: Int): Z = Z(n)
      def offset(first: Int, distance: Z): Int = first + distance.toInt
    }

  implicit def boundedForLong: Bounded[Long] =
    new Bounded[Long] {
      def countable: Countable[Long] = Countable[Long]
      def upper: Option[Long] = Some(Long.MaxValue)
      def lower: Option[Long] = Some(Long.MinValue)
      def next(n: Long): Long = n + 1L
      def prev(n: Long): Long = n - 1L
      def toZ(n: Long): Z = Z(n)
      def offset(first: Long, distance: Z): Long = first + distance.toLong
    }

  implicit def boundedForSafeLong: Bounded[SafeLong] =
    new Bounded[SafeLong] {
      def countable: Countable[SafeLong] = Countable[SafeLong]
      def upper: Option[SafeLong] = None
      def lower: Option[SafeLong] = None
      def next(n: SafeLong): SafeLong = n + 1L
      def prev(n: SafeLong): SafeLong = n - 1L
      def toZ(n: SafeLong): Z = n
      def offset(first: SafeLong, distance: Z): SafeLong = first + distance
    }
}
