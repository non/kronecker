package kronecker

/**
 * Table-based Function1 definition.
 *
 * The `default` value MUST NOT appear in `table`, or else countable
 * instances using this will be broken.
 *
 * Instances of this class have stable equality/hashCode based on the
 * table and default value.
 */
final case class TableFunction[A, B](table: Map[A, B], default: B) extends Function1[A, B] {
  def apply(a: A): B = table.getOrElse(a, default)
}
