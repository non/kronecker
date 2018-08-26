package kronecker
package instances

import scala.annotation.tailrec

object CMap {
  def apply[K, V](evk: Countable[K], evv: Countable[V]): Countable[Map[K, V]] =
    (evk.cardinality.value, evv.cardinality.value) match {
      case (_, Some(szv)) =>
        val n = szv + 1
        new CMap(evk, evv, i => i /% n)
      case (_, None) =>
        val k = 4
        val mask = (1 << k) - 1
        new CMap(evk, evv, i => Coding.read(i, mask, k))
    }
}

class CMap[K, V](evk: Countable[K], evv: Countable[V], splitIndex: Z => (Z, Z)) extends Countable[Map[K, V]] {
  val evo: Countable[Option[V]] = new COption(evv)
  val cardinality: Card = evo.cardinality ** evk.cardinality

  def get(index: Z): Option[Map[K, V]] = {
    @tailrec def loop(index0: Z, keyIndex: Z, m0: Map[K, V]): Map[K, V] =
      if (index0.isZero) {
        m0
      } else if (evk.cardinality.isMax(keyIndex)) {
        evo.get(index0).get match {
          case Some(v) => m0.updated(evk.get(keyIndex).get, v)
          case None => m0
        }
      } else {
        val (index1, valIndex) = splitIndex(index0)
        loop(index1, keyIndex + 1, evo.get(valIndex).get match {
          case Some(v) => m0.updated(evk.get(keyIndex).get, v)
          case None => m0
        })
      }
    if (cardinality.contains(index)) {
      Some(loop(index, Z.zero, Map.empty))
    } else None
  }
}

// TODO Indexable[Map[K, V]]
