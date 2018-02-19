package kronecker
package instances

// Left(0), Left(1), ..., Right(0), Right(1), ...
case class FFEither[A, B](eva: Finite[A], evb: Finite[B]) extends Finite[Either[A, B]] {
  val size: Z = eva.size + evb.size
  def get(index: Z): Option[Either[A, B]] =
    if (index < eva.size) eva.get(index).map(Left(_))
    else evb.get(index - eva.size).map(Right(_))
}

// Left(0), Left(1), ..., Right(0), Right(1), ...
case class FIEither[A, B](eva: Finite[A], evb: Infinite[B]) extends Infinite[Either[A, B]] {
  def apply(index: Z): Either[A, B] =
    if (index < eva.size) Left(eva.get(index).get)
    else Right(evb(index - eva.size))
}

// Left(0), Right(0), Left(1), Right(1), ...
case class IIEither[A, B](eva: Infinite[A], evb: Infinite[B]) extends Infinite[Either[A, B]] {
  def apply(index: Z): Either[A, B] =
    if (index.isEven) Left(eva(index >> 1)) else Right(evb(index >> 1))
}
