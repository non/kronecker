package kronecker
package instances

object CEither {

  def rev[A, B]: Either[A, B] => Either[B, A] =
    _ match {
      case Left(a) => Right(a)
      case Right(b) => Left(b)
    }

  def apply[A, B](eva: Countable[A], evb: Countable[B]): Countable[Either[A, B]] =
    (eva.cardinality.value, evb.cardinality.value) match {
      case (Some(sz), _) =>
        new CFEither(eva, sz, evb)
      case (None, Some(sz)) =>
        new CFEither(evb, sz, eva).translate(rev)
      case (None, None) =>
        new CIEither(eva, evb)
    }
}

// Left(0), Left(1), ... Left(sz - 1), Right(0), Right(1), ...
class CFEither[A, B](eva: Countable[A], sz: Z, evb: Countable[B]) extends Countable[Either[A, B]] {
  val cardinality = eva.cardinality + evb.cardinality
  def get(index: Z): Option[Either[A, B]] =
    if (index < sz) eva.get(index).map(Left(_))
    else evb.get(index - sz).map(Right(_))
}

// Left(0), Right(0), Left(1), Right(1), ...
class CIEither[A, B](eva: Countable[A], evb: Countable[B]) extends Countable[Either[A, B]] {
  val cardinality = eva.cardinality + evb.cardinality
  def get(index: Z): Option[Either[A, B]] =
    if (index.isEven) eva.get(index >> 1).map(Left(_))
    else evb.get(index >> 1).map(Right(_))
}

object NEither {

  import CEither.rev

  def apply[A, B](eva: Indexable[A], evb: Indexable[B]): Indexable[Either[A, B]] =
    (eva.cardinality.value, evb.cardinality.value) match {
      case (Some(sz), _) =>
        new NFEither(eva, sz, evb)
      case (None, Some(sz)) =>
        new NFEither(evb, sz, eva).imap(rev)(rev)
      case (None, None) =>
        new NIEither(eva, evb)
    }
}

// Left(0), Left(1), ..., Right(0), Right(1), ...
class NFEither[A, B](eva: Indexable[A], sz: Z, evb: Indexable[B])
    extends CFEither[A, B](eva, sz, evb) with Indexable[Either[A, B]] {
  def index(e: Either[A, B]): Z =
    e match {
      case Left(a) => eva.index(a)
      case Right(b) => evb.index(b) + sz
    }
}

// Left(0), Right(0), Left(1), Right(1), ...
class NIEither[A, B](eva: Indexable[A],evb: Indexable[B])
    extends CIEither[A, B](eva, evb) with Indexable[Either[A, B]] {
  def index(e: Either[A, B]): Z =
    e match {
      case Left(a) => eva.index(a) * 2
      case Right(b) => evb.index(b) * 2 + 1
    }
}

