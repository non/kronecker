package kronecker
package test

trait Test {
  def runs: Int
  def strategy: Strategy

  def test[A](s: String)(f: A => Boolean)(implicit c: Countable[A]): Unit = {
    val p = Predicate(f)
    val res = Testing.test(c, p, strategy, runs)
    println(s"$s: $res")
  }

  def test[A, B](s: String)(f: (A, B) => Boolean)(implicit c: Countable[(A, B)]): Unit =
    test[(A, B)](s) { case (a, b) => f(a, b) }

  def test[A, B, C](s: String)(f: (A, B, C) => Boolean)(implicit c: Countable[(A, B, C)]): Unit =
    test[(A, B, C)](s) { case (a, b, c) => f(a, b, c) }
}

object Demo extends Test {

  val strategy = Strategy.FibonacciR
  val runs = 100

  def main(args: Array[String]): Unit =
    test[(Z, List[Z])]("xyz") { case (n, lst) =>
      scala.util.Try(lst.map(n / _).size == lst.size).getOrElse(false)
    }
}
