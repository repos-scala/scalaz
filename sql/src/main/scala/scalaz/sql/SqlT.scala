package scalaz
package sql

// Note this is not a monad but does have flatMap
sealed trait SqlT[F[_], A] {
  private[sql] val value: F[(Boolean, Trace, Either[SqlExceptionContext, A])] = this match {
    case SqlTImpl(x) => x
  }

  def map[B](f: A => B)(implicit F: Functor[F]): SqlT[F, B] =
    SqlTImpl(F.map(value) { case (p, t, z) => (p, t, z.right map f) })

  def flatMap[B](f: A => SqlT[F, B])(implicit F: Monad[F]): SqlT[F, B] =
    SqlTImpl(F.bind(value) {
      case (p, t, y) => y match {
        case Left(e) => F.point(p, t, Left(e))
        case Right(a) =>
          F.map(f(a).value) {
            case (q, u, z) => {
              val r = p && q
              (r, if(r) t ++ u else u, z)
            }
          }
      }
    })
}
private case class SqlTImpl[F[_], A](x: F[(Boolean, Trace, Either[SqlExceptionContext, A])]) extends SqlT[F, A]

object SqlT extends SqlTFunctions

trait SqlTFunctions {
  type Sql[A] =
  SqlT[Id, A]

  import Lens._
  import CoStateT._

  def traceSwitchTL[F[_], A](implicit F: Monad[F]): SqlT[F, A] @-@ F[Boolean] =
    lens(s => coState((k => SqlTImpl(F.map2(s.value, k){
      case ((_, b, c), x) => (x, b, c)
    }), F.map(s.value) { case (a, _, _) => a })))

  def traceTL[F[_], A](implicit F: Monad[F]): SqlT[F, A] @-@ F[Trace] =
    lens(s => coState((k => SqlTImpl(F.map2(s.value, k){
      case ((a, _, c), x) => (a, x, c)
    }), F.map(s.value) { case (_, b, _) => b })))

}