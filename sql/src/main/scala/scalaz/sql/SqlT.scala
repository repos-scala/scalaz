package scalaz
package sql

sealed trait SqlT[F[_], A] {
  private[sql] val value: F[(Boolean, Trace, Either[SqlExceptionContext, A])] = this match {
    case SqlTImpl(x) => x
  }
}
private case class SqlTImpl[F[_], A](x: F[(Boolean, Trace, Either[SqlExceptionContext, A])]) extends SqlT[F, A]

object SqlT extends SqlTFunctions

trait SqlTFunctions {
  type Sql[A] =
  SqlT[Id, A]

  import Lens._
  import CoStateT._

  def traceSwitchL[F[_], A](implicit F: Monad[F]): SqlT[F, A] @-@ F[Boolean] =
    lens(s => coState((k => SqlTImpl(F.map2(s.value, k){
      case ((_, b, c), x) => (x, b, c)
    }), F.map(s.value) { case (a, _, _) => a })))

  def traceL[F[_], A](implicit F: Monad[F]): SqlT[F, A] @-@ F[Trace] =
    lens(s => coState((k => SqlTImpl(F.map2(s.value, k){
      case ((a, _, c), x) => (a, x, c)
    }), F.map(s.value) { case (_, b, _) => b })))

}