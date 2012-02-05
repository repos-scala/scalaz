package scalaz
package sql

import SqlT._

// Note this is not a monad but does have flatMap
sealed trait SqlT[F[_], A] {
  private[sql] val value: F[(Boolean, Trace, SqlExceptionContextOr[A])] = this match {
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

  def toggleTrace(implicit F: Functor[F]): SqlT[F, A] =
    SqlTImpl(F.map(value) { case (p, t, z) => (!p, t, z) })

  def traceOn(implicit F: Functor[F]): SqlT[F, A] =
    SqlTImpl(F.map(value) { case (_, t, z) => (true, t, z) })

  def traceOff(implicit F: Functor[F]): SqlT[F, A] =
    SqlTImpl(F.map(value) { case (_, t, z) => (false, t, z) })

  def isTrace(implicit F: Functor[F]): F[Boolean] =
    F.map(value) { case (p, _, _) => p }

  def get(implicit F: Functor[F]): OptionT[F, A] =
    OptionT(F.map(value) { case (_, _, y) => y.right.toOption })

  def |(a: => A)(implicit F: Functor[F]): F[A] =
    F.map(value) { case (_, _, y) => y.right getOrElse a }

  def isValid(implicit F: Functor[F]): F[Boolean] =
    F.map(value) { case (_, _, y) => y.isRight }

  def mapValue(k: A => A)(implicit F: Functor[F]): SqlT[F, A] =
    SqlTImpl(F.map(value) { case (p, t, z) => (p, t, z.right map k) })

  def withValue(k: A => SqlExceptionContextOr[A])(implicit F: Functor[F]): SqlT[F, A] =
    SqlTImpl(F.map(value) { case (p, t, z) => (p, t, z.right flatMap k) })

  def exception(implicit F: Functor[F]): OptionT[F, SqlExceptionContext] =
    OptionT(F.map(value) { case (_, _, y) => y.left.toOption })

  def exceptionOr(x: => SqlExceptionContext)(implicit F: Functor[F]): F[SqlExceptionContext] =
    F.map(value) { case (_, _, y) => y.left getOrElse x }

  def isException(implicit F: Functor[F]): F[Boolean] =
    F.map(value) { case (_, _, y) => y.isLeft }

  def mapException(k: SqlExceptionContext => SqlExceptionContext)(implicit F: Functor[F]): SqlT[F, A] =
    SqlTImpl(F.map(value) { case (p, t, z) => (p, t, z.left map k) })

  def withException(k: SqlExceptionContext => SqlExceptionContextOr[A])(implicit F: Functor[F]): SqlT[F, A] =
    SqlTImpl(F.map(value) { case (p, t, z) => (p, t, z.left flatMap k) })

  def trace(implicit F: Functor[F]): F[Trace] =
    F.map(value) { case (_, t, _) => t }

  // Note: O(n)
  def traceList(implicit F: Functor[F]): F[List[TraceOp]] =
    F.map(trace)(_.list)

}
private case class SqlTImpl[F[_], A](x: F[(Boolean, Trace, Either[SqlExceptionContext, A])]) extends SqlT[F, A]

object SqlT extends SqlTFunctions

trait SqlTFunctions {
  type Sql[A] =
  SqlT[Id, A]

  type SqlExceptionContextOr[A] =
  Either[SqlExceptionContext, A]

  import Lens._
  import CoStateT._

  def sqlTraceSwitchL[F[_], A](implicit F: Monad[F]): SqlT[F, A] @-@ F[Boolean] =
    lens(s => coState((k => SqlTImpl(F.map2(s.value, k){
      case ((_, b, c), x) => (x, b, c)
    }), F.map(s.value) { case (a, _, _) => a })))

  def sqlTraceL[F[_], A](implicit F: Monad[F]): SqlT[F, A] @-@ F[Trace] =
    lens(s => coState((k => SqlTImpl(F.map2(s.value, k){
      case ((a, _, c), x) => (a, x, c)
    }), F.map(s.value) { case (_, b, _) => b })))

  def sqlValueL[F[_], A](implicit F: Monad[F]): SqlT[F, A] @-@ F[SqlExceptionContextOr[A]] =
    lens(s => coState((k => SqlTImpl(F.map2(s.value, k){
      case ((a, b, _), x) => (a, b, x)
    }), F.map(s.value) { case (_, _, c) => c })))

}
