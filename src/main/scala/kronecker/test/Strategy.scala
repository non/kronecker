package kronecker
package test

import scala.util.Random

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

  object FibonacciR extends Strategy {
    case class State(lower: Z, upper: Z, st: Pcg32.State)
    def init: State = State(Z.zero, Z.one, Random.nextLong)
    def next(st: State): (State, Z) = {
      val State(x0, y, pst0) = st
      if (x0 <= 3) {
        (State(y, x0 + y, pst0), x0)
      } else {
        val (pst1, x1) = Pcg32.run(pst0, x0)
        (State(y, x1 + y, pst1), x0)
      }
    }
  }
}
