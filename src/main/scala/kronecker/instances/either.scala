package kronecker
package instances

// Left(0), Left(1), ..., Right(0), Right(1), ...
class CFFEither[A, B](
  eva: Countable.Finite[A],
  evb: Countable.Finite[B]
) extends Countable.Finite[Either[A, B]] {
  val size: Z = eva.size + evb.size
  def get(index: Z): Option[Either[A, B]] =
    if (index < eva.size) eva.get(index).map(Left(_))
    else evb.get(index - eva.size).map(Right(_))
}

// Left(0), Left(1), ..., Right(0), Right(1), ...
class CFIEither[A, B](
  eva: Countable.Finite[A],
  evb: Countable.Infinite[B]
) extends Countable.Infinite[Either[A, B]] {
  def apply(index: Z): Either[A, B] =
    if (index < eva.size) Left(eva.get(index).get)
    else Right(evb(index - eva.size))
}

// Left(0), Right(0), Left(1), Right(1), ...
class CIIEither[A, B](
  eva: Countable.Infinite[A],
  evb: Countable.Infinite[B]
) extends Countable.Infinite[Either[A, B]] {
  def apply(index: Z): Either[A, B] =
    if (index.isEven) Left(eva(index >> 1)) else Right(evb(index >> 1))
}

// Left(0), Left(1), ..., Right(0), Right(1), ...
class NFFEither[A, B](
  eva: Indexable.Finite[A],
  evb: Indexable.Finite[B]
) extends CFFEither[A, B](eva, evb) with Indexable.Finite[Either[A, B]] {
  def index(e: Either[A, B]): Z =
    e match {
      case Left(a) => eva.index(a)
      case Right(b) => evb.index(b) + eva.size
    }
}

// Left(0), Left(1), ..., Right(0), Right(1), ...
class NFIEither[A, B](
  eva: Indexable.Finite[A],
  evb: Indexable.Infinite[B]
) extends CFIEither[A, B](eva, evb) with Indexable.Infinite[Either[A, B]] {
  def index(e: Either[A, B]): Z =
    e match {
      case Left(a) => eva.index(a)
      case Right(b) => evb.index(b) + eva.size
    }
}

// Left(0), Right(0), Left(1), Right(1), ...
class NIIEither[A, B](
  eva: Indexable.Infinite[A],
  evb: Indexable.Infinite[B]
) extends CIIEither[A, B](eva, evb) with Indexable.Infinite[Either[A, B]] {
  def index(e: Either[A, B]): Z =
    e match {
      case Left(a) => eva.index(a) * 2
      case Right(b) => evb.index(b) * 2 + 1
    }
}

