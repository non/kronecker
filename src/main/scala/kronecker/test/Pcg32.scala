package kronecker
package test

import scala.annotation.tailrec

object Pcg32 {

  type State = Long

  final val mul = 6364136223846793005L
  final val inc = 1442695040888963407L

  final def init(seed: Long): State =
    next(seed + inc)

  final def next(st: State): State =
    st * mul + inc

  final def extract(st: State): Int =
    rotr32(((st ^ (st >>> 18)) >>> 27).toInt, (st >>> 59).toInt)

  final def run(st: State): (State, Int) =
    (next(st), extract(st))

  final def run(st: State, limit: Int): (State, Int) = {
    require(limit > 0)
    @tailrec def loop(st0: State): (State, Int) = {
      val n = extract(st0)
      val st1 = next(st0)
      val rem = n % limit
      if (n - rem + (limit - 1) >= 0) (st1, rem) else loop(st1)
    }
    loop(st)
  }

  final def run(st0: State, limit: Z): (State, Z) = {
    require(limit > 0, s"$limit")
    val n = limit.bitLength
    val q = n / 32
    val r = n % 32
    @tailrec def loop(st0: State, acc0: Z, i: Int): (State, Z) =
      if (i == 0) (st0, acc0)
      else {
        val (st1, x) = run(st0)
        val acc1 = (acc0 << 32) | (x & 0xffffffffL)
        loop(st1, acc1, i - 1)
      }
    if (r == 0) {
      loop(st0, Z.zero, q)
    } else {
      val (st1, acc) = run(st0, r)
      loop(st1, acc, q)
    }
  }

  final def rotr32(x: Int, r: Int): Int =
    (x >>> r) | (x << (-r & 31))
}
