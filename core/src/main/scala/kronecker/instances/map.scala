package kronecker
package instances

import scala.annotation.tailrec

object CMap {
  def apply[K, V](evk: Countable[K], evv: Countable[V]): Countable[Map[K, V]] =
    (evk.cardinality.value, evv.cardinality.value) match {
      case (_, Some(szv)) => new FCMap(evk, evv, szv)
      case (_, None) => new ICMap(evk, evv)
    }

  class FCMap[K, V](evk: Countable[K], evv: Countable[V], szv: Z) extends Countable[Map[K, V]] {
    val evo: Countable[Option[V]] = new COption(evv)
    val szo: Z = szv + 1
    val cardinality: Card = evo.cardinality ** evk.cardinality

    def get(index: Z): Option[Map[K, V]] = {
      @tailrec def loop(index0: Z, keyIndex: Z, m0: Map[K, V]): Map[K, V] =
        if (index0.isZero || !evk.cardinality.contains(keyIndex)) m0
        else {
          val (index1, valIndex) = index0 /% szo
          evo.get(valIndex).get match {
            case Some(v) =>
              loop(index1, keyIndex + 1, m0.updated(evk.get(keyIndex).get, v))
            case None =>
              loop(index1, keyIndex + 1, m0)
          }
        }
      if (cardinality.contains(index)) {
        Some(loop(index, Z.zero, Map.empty))
      } else None
    }
  }

  class ICMap[K, V](evk: Countable[K], evv: Countable[V]) extends Countable[Map[K, V]] {
    val evo: Countable[Option[V]] = new COption(evv)
    val cardinality: Card = evo.cardinality ** evk.cardinality

    def get(index: Z): Option[Map[K, V]] = Some({
      val k = 4
      val mask = (1 << k) - 1
      @tailrec def loop(index0: Z, keyIndex: Z, m0: Map[K, V]): Map[K, V] =
        if (index0.isZero) {
          m0
        } else if(evk.cardinality.isMax(keyIndex)) {
          evo.get(index0).get match {
            case Some(v) => m0.updated(evk.get(keyIndex).get, v)
            case None => m0
          }
        } else {
          val (valIndex, index1) = Coding.read(index0, mask, k)
          loop(index1, keyIndex + 1, evo.get(valIndex).get match {
            case Some(v) => m0.updated(evk.get(keyIndex).get, v)
            case None => m0
          })
        }
      loop(index, Z.zero, Map.empty)
    })
  }
}
