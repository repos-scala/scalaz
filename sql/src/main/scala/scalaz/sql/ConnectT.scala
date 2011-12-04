package scalaz
package sql


sealed trait ConnectT[F[_], A] {
  def apply(c: Connection): F[A]
}