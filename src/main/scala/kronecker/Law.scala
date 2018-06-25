package kronecker

import scala.util.control.NonFatal
import shapeless.HList

case class Law[H <: HList](name: String, c: Countable[H], f: H => Predicate) {

  def check(h: H): Law.Result[H] =
    try {
      f(h).result match {
        case Predicate.Result.True =>
          Law.Result.Unfalsified(1, 0)
        case Predicate.Result.Skip =>
          Law.Result.Unfalsified(0, 1)
        case r @ Predicate.Result.False(_) =>
          Law.Result.Falsified(h, r, None)
        case r @ Predicate.Result.Error(_) =>
          Law.Result.Falsified(h, r, None)
      }
    } catch { case NonFatal(e) =>
      val c = Predicate.Result.Error(e)
      Law.Result.Falsified(h, c, None)
    }

  def check(index: Z): Law.Result[H] =
    c.get(index) match {
      case Some(h) => check(h).addIndex(index)
      case None => Law.Result.Unfalsified(0, 1)
    }
}

object Law {

  sealed abstract class Result[+A] { lhs =>
    import Result._

    def addIndex(index: Z): Result[A] =
      this match {
        case f @ Falsified(_, _, _) => f.copy(maybeIndex = Some(index))
        case u @ Unfalsified(_, _) => u
      }

    def +[A1 >: A](rhs: Result[A1]): Result[A1] =
      (lhs, rhs) match {
        case (f0 @ Falsified(_, _, _), _) => f0
        case (_, f1 @ Falsified(_, _, _)) => f1
        case (Unfalsified(p0, s0), Unfalsified(p1, s1)) => Unfalsified(p0 + p1, s0 + s1)
      }
  }

  object Result {
    case class Unfalsified(passed: Int, skipped: Int) extends Result[Nothing]
    case class Falsified[+A](example: A, result: Predicate.Counterexample, maybeIndex: Option[Z]) extends Result[A]
  }
}
