package object kronecker {
  type N = spire.math.Natural
  val N = spire.math.Natural

  type Z = spire.math.SafeLong
  val Z = spire.math.SafeLong

  type Q = spire.math.Rational
  val Q = spire.math.Rational

  type R = spire.math.Real
  val R = spire.math.Real

  def floor(x: R): Z = x.toRational.toSafeLong
}
