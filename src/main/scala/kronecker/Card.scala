package kronecker

import spire.algebra.{Order, Rig}

sealed trait Card { lhs =>

  import Card._

  def value: Option[Z] =
    this match {
      case Finite(n) => Some(n)
      case Infinite => None
    }

  def compare(rhs: Card): Int =
    (lhs, rhs) match {
      case (c1, c2) if c1 == c2 => 0
      case (_, Infinite) => -1
      case (Infinite, _) => 1
      case (Finite(x), Finite(y)) => x compare y
    }

  def contains(i: Z): Boolean =
    if (i < 0) false
    else this match {
      case Infinite => true
      case Finite(n) => i < n
    }

  def <(rhs: Card): Boolean = (lhs compare rhs) < 0
  def <=(rhs: Card): Boolean = (lhs compare rhs) <= 0
  def >(rhs: Card): Boolean = (lhs compare rhs) > 0
  def >=(rhs: Card): Boolean = (lhs compare rhs) >= 0

  def +(rhs: Card): Card =
    (lhs, rhs) match {
      case (_, Infinite) => Infinite
      case (Infinite, _) => Infinite
      case (Finite(x), Finite(y)) => Finite(x + y)
    }

  def *(rhs: Card): Card =
    (lhs, rhs) match {
      case (_, Infinite) => Infinite
      case (Infinite, _) => Infinite
      case (Finite(x), Finite(y)) => Finite(x * y)
    }
}

object Card {

  def apply(n: Z): Card = Finite(n)

  case object Infinite extends Card
  case class Finite private (size: Z) extends Card {
    require(size >= 0)
  }

  val Zero = Finite(Z.zero)
  val One = Finite(Z.one)

  implicit object CardAlgebra extends Order[Card] with Rig[Card] {
    def compare(x: Card, y: Card): Int = x compare y
    def zero: Card = Zero
    def one: Card = One
    def plus(x: Card, y: Card): Card = x + y
    def times(x: Card, y: Card): Card = x * y
  }
}
