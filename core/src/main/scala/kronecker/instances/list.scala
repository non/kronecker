package kronecker
package instances

import scala.annotation.tailrec

object CList {
  def apply[A](ev: Countable[A]): Countable[List[A]] =
    ev.cardinality.value match {
      case Some(sz) => new CFList(ev, sz)
      case None => new CIList(ev)
    }
}

// this only works if the A type is finite. A types that are infinite
// require a different strategy than the lexicographic order (since
// you can never finish enumerating the length=1 lists, you need to
// diagonalize between enumerating lists of various lengths).
class CFList[A](ev: Countable[A], sz: Z) extends Countable[List[A]] {
  def cardinality: Card =
    Card.infinite
  def get(index: Z): Option[List[A]] =
    Some({
      val bldr = List.newBuilder[A]
      var curr = index
      while (!curr.isZero) {
        val (q, r) = (curr - 1) /% sz
        bldr += ev.get(r).get
        curr = q
      }
      bldr.result
    })
}

class CIList[A](ev: Countable[A]) extends Countable[List[A]] {
  val k = 4
  val mask = (1 << k) - 1

  val cardinality: Card = Card.infinite

  def get(index: Z): Option[List[A]] =
    Some({
      val bldr = List.newBuilder[A]
      var n = index
      while (!n.isZero) {
        val (valIndex, rest) = decode(n, mask, k)
        bldr += ev.get(valIndex).get
        n = rest
      }
      bldr.result
    })

  // decode states (also used for encode)
  final val A = 1 // on m goto B, else goto C
  final val B = 2 // on 1 stay in B, on m goto D, else goto C (semi-carry)
  final val C = 3 // on m goto D, else stay in C
  final val D = 4 // on m or 1 stay in D, else goto C (carry)

  def decode(n: Z, m: Int, k: Int): (Z, Z) = {
    @tailrec def loop(curr: Z, acc: Z, mult: Z, state: Int): (Z, Z) = {
      val q = curr >> k
      val r = (curr & m).toInt
      val isLast = q.isZero
      val isEnd = r == 0

      if (isEnd) {
        (if (state == B || state == D) acc + mult else acc, q)
      } else {
        val (acc2, st2) =
          if (r == m) (acc, if (state == A) B else D)
          else if (r == 1) (acc + mult, if (state == B) B else if (state == D) D else C)
          else (acc + (r * mult), C)

        val mult2 = mult * m

        if (isLast) {
          (if (st2 == D) acc2 + mult2 else acc2, Z.zero)
        } else {
          loop(q, acc2, mult2, st2)
        }
      }
    }
    loop(n, Z.zero, Z.one, A)
  }
}

object NList {
  def apply[A](ev: Indexable[A]): Indexable[List[A]] =
    ev.cardinality.value match {
      case Some(sz) => new NFList(ev, sz)
      case None => new NIList(ev)
    }
}

class NFList[A](ev: Indexable[A], sz: Z) extends CFList(ev, sz) with Indexable[List[A]] {
  def index(lst: List[A]): Z = {
    def loop(as: List[A], acc: Z, mult: Z): Z =
      as match {
        case head :: tail =>
          loop(tail, acc + (ev.index(head) + 1) * mult, mult * sz)
        case Nil =>
          acc
      }
    if (lst.isEmpty) Z.zero
    else loop(lst, Z.zero, Z.one)
  }
}

class NIList[A](ev: Indexable[A]) extends CIList(ev) with Indexable[List[A]] {
  val zmask = Z(mask)

  def index(lst: List[A]): Z = {
    @tailrec def loop(index: Z, shift: Int, as: List[A]): Z =
      as match {
        case Nil =>
          index
        case head :: tail =>
          val (part2, shift2) = encode(head, shift, tail.isEmpty)
          loop(index + part2, shift2, tail)
      }
    loop(Z.zero, 0, lst)
  }


  def encode(a: A, shift0: Int, isLast: Boolean): (Z, Int) = {
    // in the loop, we step through the current value, looking at its
    // least-significant digit base-mask, and encoding it in
    // base-(mask+1). we need to make sure zero digits are encoding as
    // the mask, since actual zeros are used to signal the end of a
    // particular element (value-index).
    //
    // state transitions mirror those from decode: D means we will
    // carry no matter what, B means we'll only carry if we're not on
    // the final element, and C means we won't carry. (it should not
    // be possible to end up in state A when we're done with this
    // element.)
    @tailrec def loop(curr: Z, acc: Z, shift: Int, state: Int): (Z, Int) = {
      val (q, r) = curr /% zmask
      val (digit, st) =
        if (r.isZero) (zmask, if (state == A) B else D)
        else if (r.isOne) (r, if (state == B) B else if (state == D) D else C)
        else (r, C)

      if (q.isZero) {
        if (st == D || (!isLast && st == B)) (acc, shift + k)
        else (acc + (digit << shift), shift + k + k)
      } else {
        loop(q, acc + (digit << shift), shift + k, st)
      }
    }

    val vindex = ev.index(a)
    if (vindex.isZero) {
      // our value-index is zero, so we'll either use the record
      // terminator (zero) or a logical zero (mask) to signal that.
      if (isLast) {
        // we need to use a mask digit, since a zero digit isn't
        // meaningful in the "most significant" position.
        (zmask << shift0, shift0 + k)
      } else {
        // we're not at the end yet, so use a record terminator.
        (Z.zero, shift0 + k)
      }
    } else {
      // we have a non-zero value-index, so go ahead and encode it.
      loop(vindex, Z.zero, shift0, A)
    }
  }
}

// final digit only
//
// do not carry ^3, ^13, ^113, etc.
// do carry ^33, ^133, ^311, ^312, ^313, ^333, ^1333, etc.
//
// e.g.
//    1 =     1 =  1
//    2 =     2 =  2
//    3 =     0 =  0
//   11 =    11 =  4
//   12 =    12 =  5
//   13 =    10 =  3
//   21 =    21 =  7
//   22 =    22 =  8
//   23 =    20 =  6
//   31 =   101 = 10
//   32 =   102 = 11
//   33 =   100 =  9
//  111 =   111 = 13
//  112 =   112 = 14
//  113 =   110 = 12
//  121 =   121 = 16
//  122 =   122 = 17
//  123 =   120 = 15
//  131 =  1101 = 37
//  132 =  1102 = 38
//  133 =  1100 = 36
//  211 =   211 = 22
//  212 =   212 = 23
//  213 =   210 = 21
//  221 =   221 = 25
//  222 =   222 = 26
//  223 =   220 = 24
//  231 =   201 = 19
//  232 =   202 = 20
//  233 =   200 = 18
//  311 =  1011 = 31
//  312 =  1012 = 32
//  313 =  1010 = 30
//  321 =  1021 = 34
//  322 =  1022 = 35
//  323 =  1020 = 33
//  331 =  1001 = 28
//  332 =  1002 = 29
//  333 =  1000 = 27
// 1111 =  1111 = 40
// 1112 =  1112 = 41
// 1113 =  1110 = 39
// 1121 =  1121 = 43
// 1122 =  1122 = 44
// 1123 =  1120 = 42
// 1131 = 11101 = 118
// 1132 = 11102 = 119
// 1133 = 11100 = 117
// 1211 =  1211 = 49
// 1212 =  1212 = 50
// 1213 =  1210 = 48
// 1221 =  1221 = 52
// 1222 =  1222 = 53
// 1223 =  1220 = 51
// 1231 =  1201 = 46
// 1232 =  1202 = 47
// 1233 =  1200 = 45
// 1311 = 11011 = 112
// 1312 = 11012 = 113
// 1313 = 11010


// example codings
//
//    0 = ,       = empty list * no carry
//   01 = 1,      = List(1)
//   02 = 2,      = List(2)
//   03 = 0,      = List(0)
//  010 = 0,1,    = List(0, 1)
//  011 = 11,     = List(4)
//  012 = 12,     = List(5)
//  013 = 10,     = List(3) * no carry
//  020 = 0,2,    = List(0, 2)
//  021 = 21,     = List(7)
//  022 = 22,     = List(8)
//  023 = 20,     = List(6)
//  030 = 0,0,   = List(0, 0)
//  031 = 101,    = List(10)
//  032 = 102,    = List(11)
//  033 = 100,    = List(9)
// 0100 = 0,0,1,  = List(0, 0, 1)
// 0101 = 1,1,    = List(1, 1)
// 0102 = 2,1,    = List(2, 1)
// 0103 = 10,1,   = List(3, 1)
// 0110 = 0,11,   = List(0, 4)
// 0111 = 111,    = List(13)
// 0112 = 112,    = List(14)
// 0113 = 110,    = List(12) * no carry
// 0120 = 0,12    = List(0, 5)
// 0121 = 121,    = List(16)
// 0122 = 122,    = List(17)
// 0123 = 120,    = List(15)
// 0130 = 0,110   = List(0, 12)
// 0131 = 1101,   = List(37)
// 0132 = 1102,   = List(38)
// 0133 = 1100,   = List(36)
// 0200 = 0,0,2,  = List(0, 0, 2)
// 0201 = 1,2,    = List(1, 2)
// 0202 = 2,2,    = List(2, 2)
// 0203 = 10,2,   = List(3, 2)
// 0210 = 0,21,   = List(0, 7)
// 0211 = 211,    = List(22)
// 0212 = 212,    = List(23)
// 0213 = 210,    = List(21)
// 0220 = 0,22,   = List(0, 8)
// 0221 = 221,    = List(25)
// 0222 = 222,    = List(26)
// 0223 = 220,    = List(27)
// 0230 = 0,20,   = List(0, 6)
// 0231 = 0,201,  = List(0, 19)
// 0232 = 0,202,  = List(0, 20)
// 0233 = 0,200,  = List(0, 18)
