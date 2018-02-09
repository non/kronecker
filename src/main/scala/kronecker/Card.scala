package kronecker

sealed trait Card { lhs =>

  import Card._

  def value: Option[Z] =
    this match {
      case Finite(n) => Some(n)
      case Zero => Some(Z.zero)
      case Infinite => None
    }

  def compare(rhs: Card): Int =
    (lhs, rhs) match {
      case (c1, c2) if c1 == c2 => 0
      case (_, Zero) => 1
      case (Zero, _) => -1
      case (_, Infinite) => -1
      case (Infinite, _) => 1
      case (Finite(x), Finite(y)) => x compare y
    }

  def contains(i: Z): Boolean =
    if (i < 0) false
    else this match {
      case Infinite => true
      case Zero => false
      case Finite(n) => i < n
    }

  def <(rhs: Card): Boolean = (lhs compare rhs) < 0
  def <=(rhs: Card): Boolean = (lhs compare rhs) <= 0
  def >(rhs: Card): Boolean = (lhs compare rhs) > 0
  def >=(rhs: Card): Boolean = (lhs compare rhs) >= 0

  def +(rhs: Card): Card =
    (lhs, rhs) match {
      case (_, Zero) => lhs
      case (Zero, _) => rhs
      case (_, Infinite) => Infinite
      case (Infinite, _) => Infinite
      case (Finite(x), Finite(y)) => Finite(x + y)
    }

  def -(rhs: Card): Card =
    (lhs, rhs) match {
      case (_, Zero) => lhs
      case (Zero, _) => Zero
      case (Infinite, Infinite) => sys.error("!")
      case (_, Infinite) => Zero
      case (Infinite, _) => Infinite
      case (Finite(x), Finite(y)) => Card(x - y)
    }

  def *(rhs: Card): Card =
    (lhs, rhs) match {
      case (_, Zero) => Zero
      case (Zero, _) => Zero
      case (_, Infinite) => Infinite
      case (Infinite, _) => Infinite
      case (Finite(x), Finite(y)) => Finite(x * y)
    }
}

object Card {

  def apply(n: Z): Card =
    if (n <= 0) Zero else Finite(n)

  case object Zero extends Card
  case object Infinite extends Card
  case class Finite private (size: Z) extends Card
}
