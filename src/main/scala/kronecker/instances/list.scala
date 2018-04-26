package kronecker
package instances

import scala.annotation.tailrec

// this only works if the A type is finite. A types that are
// infinite require a different strategy than the lexicographic
// order (since you can never "finish" the length=1 lists you have
// to interleave larger ones).
class LexicographicList[A](ev: Countable.Finite[A]) extends Countable.Infinite[List[A]] {
  def apply(index: Z): List[A] = {
    val w = ev.size
    @tailrec def loop(len: Int, i: Z): (Int, Z) = {
      val j = i + ev.size.pow(len)
      if (j <= index) loop(len + 1, j) else (len, index - i)
    }
    val (len, i) = loop(0, Z.zero)
    @tailrec def build(len: Int, i: Z, as: List[A]): List[A] =
      if (len <= 0) as
      else {
        val (j, k) = i /% ev.size
        build(len - 1, j, ev.get(k).get :: as)
      }
    build(len, i, Nil)
  }
}


class CodedList[A](ev: Countable.Infinite[A]) extends Countable.Infinite[List[A]] {
  val k = 2
  def apply(index: Z): List[A] = {
    val mask = (1 << k) - 1
    val bldr = List.newBuilder[A]
    var n = index
    while (!n.isZero) {
      val (valIndex, rest) = Functional.readCoding(n, mask, k)
      bldr += ev(valIndex)
      n = rest
    }
    bldr.result
  }
}

class NCodedList[A](ev: Indexable.Infinite[A]) extends CodedList(ev) with Indexable.Infinite[List[A]] {
  def index(lst: List[A]): Z =
    Functional.writeCoding(lst.map(ev.index), k)
}
