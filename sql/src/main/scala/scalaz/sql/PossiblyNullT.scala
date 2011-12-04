package scalaz
package sql


sealed trait PossiblyNullT[F[_], A] {
  val toOptionT: OptionT[F, A]
}
