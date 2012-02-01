package scalaz
package sql

sealed trait SqlT[F[_], A] {

}
private case class SqlTImpl[F[_], A](x: F[(Boolean, Trace, Either[SqlExceptionContext, A])]) extends SqlT[F, A]

object SqlT extends SqlTFunctions

trait SqlTFunctions {
  type Sql[A] =
  SqlT[Id, A]
}