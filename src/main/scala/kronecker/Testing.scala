package kronecker

import scala.annotation.tailrec

abstract class Strategy { self =>
  type State
  def init: State
  def next(st: State): (State, Z)

  def iterator: Iterator[Z] =
    new Iterator[Z] {
      var st0: State = self.init
      def hasNext(): Boolean = true
      def next(): Z = {
        val (st1, index1) = self.next(st0)
        st0 = st1
        index1
      }
    }
}

object Strategy {

  object Increment extends Strategy {
    type State = Z
    def init: Z = Z.zero
    def next(st: Z): (Z, Z) = (st + 1, st)
  }

  object Fibonacci extends Strategy {
    type State = (Z, Z)
    def init: (Z, Z) = (Z.zero, Z.one)
    def next(st: (Z, Z)): ((Z, Z), Z) = {
      val (x, y) = st
      ((y, x + y), x)
    }
  }
}

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
