package kronecker

import spire.algebra.{Order, Rig}

sealed trait Card { lhs =>

  import Card._

  def value: Option[Z] =
    this match {
      case Finite(n) => Some(n)
      case _ => None
    }

  def contains(i: Z): Boolean =
    if (i < 0) false
    else this match {
      case Finite(n) => i < n
      case _ => true
    }

  def isMax(i: Z): Boolean =
    this match {
      case Finite(n) => i == n
      case _ => false
    }

  def +(rhs: Card): Card =
    (lhs, rhs) match {
      case (_, Infinite) => Infinite
      case (Infinite, _) => Infinite
      case (x: Semifinite, y: Semifinite) => plus(x, y)
    }

  def *(rhs: Card): Card =
    (lhs, rhs) match {
      case (Zero, _) => Zero
      case (_, Zero) => Zero
      case (_, Infinite) => Infinite
      case (Infinite, _) => Infinite
      case (x: Semifinite, y: Semifinite) => times(x, y)
    }

  def **(rhs: Card): Card =
    (lhs, rhs) match {
      case (_, Zero) => One
      case (Zero, _) => Zero
      case (_, One) => lhs
      case (One, _) => One
      case (_, Infinite) => Infinite
      case (Infinite, _) => Infinite
      case (x: Semifinite, y: Semifinite) => pow(x, y)
    }

  def partialCompare(rhs: Card): Double =
    (lhs, rhs) match {
      case (x, y) if x == y       =>  0.0
      case (_, Infinite)          => -1.0
      case (Infinite, _)          =>  1.0
      case (Finite(x), Finite(y)) => (x compare y).toDouble
      case (Finite(_), _)         => -1.0
      case (_, Finite(_))         =>  1.0
      case (_, _)                 => Double.NaN
    }
}

object Card {

  def apply(n: Z): Card = Finite(n)

  case object Infinite extends Card {
    override def toString: String = "∞"
  }

  // 2^6553400 has 1972770 decimal digits
  val MaxExponent: Z = Z(6553400)

  sealed abstract class Semifinite extends Card {
    override def toString: String =
      this match {
        case Finite(x) => x.toString
        case Plus(x, y) => s"($x + $y)"
        case Times(x, y) => s"($x * $y)"
        case Pow(x, y) => s"($x ** $y)"
      }
  }

  case class Plus(x: Semifinite, y: Semifinite) extends Semifinite
  case class Times(x: Semifinite, y: Semifinite) extends Semifinite
  case class Pow(x: Semifinite, y: Semifinite) extends Semifinite

  case class Finite private (size: Z) extends Semifinite {
    require(size >= 0)
  }

  val Zero: Finite = Finite(Z.zero)
  val One: Finite = Finite(Z.one)
  val Two: Finite = Finite(Z(2))

  def zero: Card = Zero
  def one: Card = One
  def two: Card = Two
  def infinite: Card = Infinite

  def plus(lhs: Semifinite, rhs: Semifinite): Semifinite =
    (lhs, rhs) match {
      case (Zero, _) => rhs
      case (_, Zero) => lhs
      case (Finite(x), Finite(y)) => Finite(x + y)
      case (Plus(x, y), z) => Plus(x, plus(y, z))
      case (Finite(x), Plus(Finite(y), z)) => Plus(Finite(x + y), z)
      case (x, y) => Plus(x, y)
    }

  def times(lhs: Semifinite, rhs: Semifinite): Semifinite =
    (lhs, rhs) match {
      case (Zero, _) => Zero
      case (_, Zero) => Zero
      case (One, _) => rhs
      case (_, One) => lhs
      case (Finite(x), Finite(y)) => Finite(x * y)
      case (Times(x, y), z: Semifinite) => Times(x, times(y, z))
      case (Finite(x), Times(Finite(y), z)) => Times(Finite(x * y), z)
      case (x, y) => Times(x, y)
    }

  def pow(lhs: Semifinite, rhs: Semifinite): Semifinite =
    (lhs, rhs) match {
      case (Finite(x), Finite(y)) => Card.semipow(x, y)
      case (_, Zero) => One
      case (Zero, _) => Zero
      case (_, One) => lhs
      case (One, _) => Zero
      case (Pow(x, y), z) => Pow(x, times(y, z))
      case (x: Semifinite, y: Semifinite) => Pow(x, y)
    }

  def semipow(base: Z, exponent: Z): Semifinite =
    if      (exponent.isZero)     Card.One
    else if (base.isZero)         Card.Zero
    else if (base.isOne)          Card.One
    else {
      // (2 ** k) approximates (base ** exponent)
      val k = exponent * base.bitLength
      if (k <= MaxExponent) Card.Finite(base.pow(exponent.toInt))
      else Pow(Card.Finite(base), Card.Finite(exponent))
    }

  implicit object CardAlgebra extends Rig[Card] {
    def zero: Card = Zero
    def one: Card = One
    def plus(x: Card, y: Card): Card = x + y
    def times(x: Card, y: Card): Card = x * y
  }
}
