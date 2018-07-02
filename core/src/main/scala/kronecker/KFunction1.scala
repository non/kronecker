// package kronecker
// 
// final case class TableFunction[A, B](table: Map[A, B], default: B) extends Function1[A, B] {
//   def apply(a: A): B = table.getOrElse(a, default)
// }
// 
// /**
//  * KFunction1 represents a Function1 that is tagged with a
//  * context-dependent `id` which describes (and uniquely-identifies)
//  * the function from other similar functions.
//  *
//  * The id's string representation will be used, as well as its
//  * equality and hash code.
//  *
//  * For example, Kronecker's function generation will tag the generated
//  * function with the indexable and countable instances as well as the
//  * index used.
//  */
// abstract class KFunction1[-A, +B](val id: AnyRef) extends Function1[A, B] {
// 
//   override def toString: String =
//     s"KFunction1($id)"
// 
//   override def equals(that: Any): Boolean =
//     that match {
//       case f: AnyRef if this eq f => true
//       case f: KFunction1[_, _] => id == f.id
//       case _ => false
//     }
// 
//   override def hashCode: Int =
//     id.hashCode
// }
