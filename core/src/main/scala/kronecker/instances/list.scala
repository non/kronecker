package kronecker
package instances

import scala.annotation.tailrec

object CList {

  /**
   * Build Countable[List[A]] using the appropriate strategy.
   *
   * See CFList for how we handle finite A types, and CIList for
   * infinite A types.
   */
  def apply[A](ev: Countable[A]): Countable[List[A]] =
    ev.cardinality.value match {
      case Some(sz) if sz.isZero => Countable.singleton(Nil)
      case Some(sz) => new CFList(ev, sz)
      case None => new CIList(ev)
    }
}

/**
 * Countable[List[A]] for finite A types.
 *
 * We encode these lists element-by-element in base-c digits, where c
 * is the cardinality of A, offset by 1 (the first element is always
 * the empty list). This ends up being equivalent to using
 * lexicographic order to generate the lists.
 *
 * For example, for List[Boolean] we have:
 *
 *    0: List()
 *    1: List(false)
 *    2: List(true)
 *    3: List(false, false)
 *    4: List(true, false)
 *    5: List(false, true)
 *    6: List(true, true)
 *    7: List(false, false, false)
 *    8: List(true, false, false)
 *    9: List(false, true, false)
 *   10: List(true, true, false)
 *
 * This strategy only works if A is finite, since for an infinite type
 * you won't ever finish enumerating the lists of length one.
 *
 * (This method assumes the cardinality of A is >= 1. If the
 * cardinality is zero, only the singleton list can be generated.)
 */
class CFList[A](ev: Countable[A], sz: Z) extends Countable[List[A]] {

  def cardinality: Card =
    Card.infinite

  def get(index: Z): Option[List[A]] = {
    val bldr = List.newBuilder[A]
    var curr = index
    while (!curr.isZero) {
      val (q, r) = (curr - 1) /% sz
      bldr += ev.get(r).get
      curr = q
    }
    Some(bldr.result())
  }
}

/**
 * Countable[List[A]] for infinite A types.
 *
 * For an infinite type Countable[A] implies a bijection with the
 * natural numbers, so we can explain how this instance works in terms
 * of natural numbers.
 *
 * The first element in the enumeration is the empty list (as in
 * CFList). For non-zero indices, we split the index into hexadecimal
 * (base-16) "list digits" to be processed, starting with the least
 * significant digit. (Hexadecimal digits are prefixed with 0x.)
 *
 * We need a way of deciding how many elements are in our list, so we
 * reserve 0x0 as a delimiter between separate elements' indices.
 *
 * Since we've reserved 0x0, we have 15 possible values to use to
 * encode each element's index (0x1 through 0xf), meaning these
 * indices are expressed in pentadecimal (base-15), which we will
 * prefix with 0p. We also need to translate our values (1-15) into
 * the appropriate pentadecimal range (0-14), which we do by treating
 * 0xf as 0p0, leaving other digits unchanged (so 0x1 is 0p1, 0x9 is
 * 0p9, and so on).
 *
 * Broadly-speaking, you can look at the hex representation of the
 * input and visually parse how to split things:
 *
 *     0x3e0f9013 -> 0x3e / 0xf9 / 0x13 -> 0p3e / 0p09 / 0p13
 *
 * However, there is a problem. 0p09 is equivalent to 0p9, but this
 * would mean that we have two ways of representing the same list
 * element. This would break our bijection, since we are required to
 * have exactly one way to represent every possible list.
 *
 * To fix this, we treat a leading 0p0 digit as though it came after a
 * 0p1 digit (we call this "carrying"). This means that 0xf9 would be
 * treated as 0p109 instead of 0p09, which fixes the problem with
 * leading 0xf, but creates a problem with leading 0x1f, since 0x1f9
 * would _also_ be interpreted as 0p109. To handle this, we will
 * continue carrying across any 0p1 digits we see after 0p0. This
 * means 0x1f9 is treated as 0p1109, 0x11f9 is treated as 0p11109, and
 * so on.
 *
 * The only other detail of carrying is that when dealing with the
 * "last" element index (i.e. the largest significant digits of the
 * hexadecimal index), we have a special treatment 0xf, 0x1f, 0x11f,
 * and so on. In those cases, we ignore the carried 1 digit. This is
 * needed because of the fact that we need to treat 0xf0... as zero,
 * since 0x0... is equivalent to 0x..., i.e. leading zeros are lost.
 *
 * The order of the list is the order we parse the element indices
 * in: least significant digits first.
 *
 * Here are some worked examples:
 *
 *      INDEX   HEX-INDEX      PENTA-ELEMS  RESULT
 *          0         0x0                .  Nil
 *          1         0x1              0p1  List(1)
 *         14         0xe              0pe  List(14)
 *         15         0xf              0p0  List(0)
 *         16        0x10          0p1,0p0  List(0, 1)
 *         17        0x11             0p11  List(16)
 *         31        0x1f             0p10  List(15)
 *         32        0x20          0p2,0p0  List(0, 2)
 *        255        0xff            0p100  List(225)
 *        256       0x100      0p1,0p0,0p0  List(0, 0, 1)
 *        257       0x101          0p1,0p1  List(1, 1)
 *        510       0x1fe           0p110e  List(3614)
 *        511       0x1ff           0p1100  List(3600)
 *        512       0x200      0p2,0p0,0p0  List(0, 0, 2)
 *        513       0x201          0p2,0p1  List(1, 2)
 *     130821     0x1ff05       0p1100,0p5  List(3600,5)
 *     395016     0x60708      0p6,0p7,0p8  List(8, 7, 6)
 *    1122053    0x111f05      0p11110,0p5  List(5, 3615)
 * 1041207315  0x3e0f9013  0p3e,0p109,0p13  List(18, 234, 59)
 *
 * (Use Countable[List[Natural]] to see this in action.)
 */
class CIList[A](ev: Countable[A]) extends Countable[List[A]] {

  protected[this] final val k = 4
  protected[this] final val mask = 0xf

  val cardinality: Card = Card.infinite

  /**
   * Get a list given its index.
   *
   * Since get(0) returns an empty list, we repeatedly extract
   * elements out of the given `index` until it is zero.
   */
  def get(index: Z): Option[List[A]] = {
    val bldr = List.newBuilder[A]
    var n = index
    while (!n.isZero) {
      val (valIndex, rest) = decode(n)
      bldr += ev.get(valIndex).get
      n = rest
    }
    Some(bldr.result())
  }

  /*
   * A-D are encode/decode states:
   *
   * encode transitions occur on m, 1, or x (wildcard).
   * all states other than A are possible end states.
   *
   *                   1
   *                  ┏━┓
   *                  ▼ ┃
   *          m  ┏━━━━━━┻━━━━━┓ m
   *      ┏━━━━━▶┃B.semi-carry┣━━━┓
   *      ┃      ┗━┳━━━━━━━━━━┛   ┃     m,1
   *      ┃        ┃              ▼    ┏━━┓
   *   ┏━━┻━━━━┓   ┃ x       x  ┏━━━━━━┻┓ ┃
   * ━▶┃A.start┃   ┗━━┓   ┏━━━━━┫D.carry┃ ┃
   *   ┗━━┳━━━━┛      ┃   ┃     ┗━━━━━━━┛ ┃
   *      ┃           ▼   ▼       ▲    ▲  ┃
   *      ┃ 1,x  ┏━━━━━━━━━━┓  m  ┃    ┗━━┛
   *      ┗━━━━━▶┃C.continue┣━━━━━┛
   *             ┗━━━━━━┳━━━┛
   *                  ▲ ┃1,x
   *                  ┗━┛
   *
   * (the decode diagram is the same, except 0 replaces m.)
   */
  final val A = 1
  final val B = 2
  final val C = 3
  final val D = 4

  /**
   * Decode a particular element out of the list's encoded index.
   *
   * This returns the element's index followed by the index of the
   * rest of the list.
   *
   * The input (`n`) is expected to be non-zero (since the list at
   * index zero has no elements, all non-zero list indices have at
   * least one element).
   */
  def decode(n: Z): (Z, Z) = {

    // loop through the blocks reading this element index. a zero
    // block means we're done with this element.
    //
    // curr is the current index being parsed and acc is the
    // accumulated element index. mult is the multiplier used for the
    // currently parsed digit, and state tracks out internal state
    // machine (A-D) which handles carrying.
    //
    //     A. start: on m goto B, else goto C
    //     B. semi-carry: on 1 stay in B, on m goto D, else goto C
    //     C. continue: on m goto D, else stay in C
    //     D. carry: on m or 1 stay in D, else goto C
    //
    // the only difference between states B and D is that on the very
    // last element to decode (i.e. the most signficiant digits of the
    // input), D will include a carried 1 digit whereas B will not. on
    // elements that aren't the last element both B and D will include
    // a carried 1 leading digit.
    @tailrec def loop(curr: Z, acc: Z, mult: Z, state: Int): (Z, Z) = {
      val q = curr >> k           // the unread parts of the index
      val r = (curr & mask).toInt // the digit to read
      val isLast = q.isZero       // is this the last element of the list?
      val isEnd = r == 0          // are we done reading the element?

      if (isEnd) {
        (if (state == B || state == D) acc + mult else acc, q)
      } else {
        val (acc2, st2) =
          if (r == mask) (acc, if (state == A) B else D)
          else if (r == 1) (acc + mult, if (state == B) B else if (state == D) D else C)
          else (acc + (r * mult), C)

        val mult2 = mult * mask

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

/**
 * Build Indexable[List[A]] using the appropriate strategy.
 *
 * Like CList, this depends on the cardinality of A.
 */
object NList {
  def apply[A](ev: Indexable[A]): Indexable[List[A]] =
    ev.cardinality.value match {
      case Some(sz) if sz.isZero => Countable.singleton(Nil)
      case Some(sz) => new NFList(ev, sz)
      case None => new NIList(ev)
    }
}

/**
 * Indexable[List[A]] for finite A types.
 *
 * NFList is the complement to CFList when A is also indexable. See
 * CFList for more information about the mapping between indices and
 * values.
 */
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

/**
 * Indexable[List[A]] for infinite A types.
 *
 * NIList is the complement to CIList when A is also indexable. See
 * CIList for more information about the mapping between indices and
 * values.
 */
class NIList[A](ev: Indexable[A]) extends CIList(ev) with Indexable[List[A]] {

  private[this] val zmask = Z(mask)

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
    // state transitions mirror those from decode, substituting zero
    // for m:
    //
    //   A: on 0 goto B, else goto C
    //   B: on 1 stay in B, on 0 goto D, else goto C
    //   C: on 0 goto D, else stay in C
    //   D: on 0 or 1 stay in D, else goto C
    //
    // D means we will carry no matter what, B means we'll only carry
    // if we're not on the final element, and C means we won't carry.
    // (it should not be possible to end up in state A when we're done
    // with this element.)

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
