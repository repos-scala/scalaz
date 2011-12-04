package scalaz
package sql


sealed trait SqlValueT[F[_], A] {
  val value: EitherT[F, SqlExceptionContext, A]
}