package kronecker
package test

import scala.annotation.tailrec

object Testing {

  sealed trait Result
  case class Success(indices: List[Z]) extends Result
  case class Failure(failure: Z) extends Result

  def test[A](c: Countable[A], p: Predicate[A], s: Strategy, n: Int): Result = {
    @tailrec def loop(tries: Int, st0: s.State, passed: List[Z]): Result =
      if (tries >= n) {
        Success(passed)
      } else {
        val (st1, i) = s.next(st0)
        c.get(i) match {
          case None => Success(passed)
          case Some(a) if p(a) => loop(tries + 1, st1, i :: passed)
          case Some(_) => Failure(i)
        }
      }
    loop(0, s.init, Nil)
  }
}
