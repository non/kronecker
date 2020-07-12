package kronecker
package refined

trait Bounded[A] {
  def upper: A
  def next(a: A): A
  def prev(a: A): A
  def lower: A
  def toZ(a: A): Z
  def offset(first: A, distance: Z): A
  final def distance(first: A, last: A): Z =
    toZ(last) - toZ(first) + Z.one
}

object Bounded {

  implicit def boundedForInt: Bounded[Int] =
    new Bounded[Int] {
      def upper: Int = Int.MaxValue
      def next(n: Int): Int = n + 1
      def prev(n: Int): Int = n - 1
      def lower: Int = Int.MinValue
      def toZ(n: Int): Z = Z(n)
      def offset(first: Int, distance: Z): Int = first + distance.toInt
    }

  implicit def boundedForLong: Bounded[Long] =
    new Bounded[Long] {
      def upper: Long = Long.MaxValue
      def next(n: Long): Long = n + 1L
      def prev(n: Long): Long = n - 1L
      def lower: Long = Long.MinValue
      def toZ(n: Long): Z = Z(n)
      def offset(first: Long, distance: Z): Long = first + distance.toLong
    }
}
