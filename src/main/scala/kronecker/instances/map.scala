package kronecker
package instances

import scala.annotation.tailrec

case class FFMap[K, V](evk: Finite[K], evv: Finite[V]) extends Finite[Map[K, V]] {
  val evo: Finite[Option[V]] = FOption(evv)

  // NOTE: powOf() will crash for unsupportedly-large cardinalities
  val size: Z = powOf(evo.size, evk.size)

  def get(index: Z): Option[Map[K, V]] = {
    @tailrec def loop(index0: Z, keyIndex: Z, m0: Map[K, V]): Map[K, V] =
      if (index0.isZero || keyIndex >= evk.size) m0
      else {
        val (index1, valIndex) = index0 /% evo.size
        evo.get(valIndex).get match {
          case Some(v) =>
            loop(index1, keyIndex + 1, m0.updated(evk.get(keyIndex).get, v))
          case None =>
            loop(index1, keyIndex + 1, m0)
        }
      }
    if (index >= size) None else Some(loop(index, Z.zero, Map.empty))
  }
}

case class IFMap[K, V](evk: Infinite[K], evv: Finite[V]) extends Infinite[Map[K, V]] {
  val evo: Finite[Option[V]] = FOption(evv)

  def apply(index: Z): Map[K, V] = {
    @tailrec def loop(index0: Z, keyIndex: Z, m0: Map[K, V]): Map[K, V] =
      if (index0.isZero) m0
      else {
        val (index1, valIndex) = index0 /% evo.size
        evo.get(valIndex).get match {
          case Some(v) =>
            loop(index1, keyIndex + 1, m0.updated(evk(keyIndex), v))
          case None =>
            loop(index1, keyIndex + 1, m0)
        }
      }
    loop(index, Z.zero, Map.empty)
  }
}

case class FIMap[K, V](evk: Finite[K], evv: Infinite[V]) extends Infinite[Map[K, V]] {
  val evo: Infinite[Option[V]] = IOption(evv)

  def apply(index: Z): Map[K, V] = {
    val k = 4
    val mask = (1 << k) - 1
    @tailrec def loop(index0: Z, keyIndex: Z, m0: Map[K, V]): Map[K, V] =
      if (index0.isZero || !evk.cardinality.contains(keyIndex)) m0
      else {
        val (valIndex, index1) = Functional.readCoding(index0, mask, k)
        evo(valIndex) match {
          case Some(v) =>
            loop(index1, keyIndex + 1, m0.updated(evk.get(keyIndex).get, v))
          case None =>
            loop(index1, keyIndex + 1, m0)
        }
      }
    loop(index, Z.zero, Map.empty)
  }
}

case class IIMap[K, V](evk: Infinite[K], evv: Infinite[V]) extends Infinite[Map[K, V]] {
  val evo: Infinite[Option[V]] = IOption(evv)

  def apply(index: Z): Map[K, V] = {
    val k = 4
    val mask = (1 << k) - 1
    @tailrec def loop(index0: Z, keyIndex: Z, m0: Map[K, V]): Map[K, V] =
      if (index0.isZero) m0
      else {
        val (valIndex, index1) = Functional.readCoding(index0, mask, k)
        evo(valIndex) match {
          case Some(v) =>
            loop(index1, keyIndex + 1, m0.updated(evk(keyIndex), v))
          case None =>
            loop(index1, keyIndex + 1, m0)
        }
      }
    loop(index, Z.zero, Map.empty)
  }
}
