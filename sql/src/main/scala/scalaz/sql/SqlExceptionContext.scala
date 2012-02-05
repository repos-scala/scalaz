package scalaz
package sql

import SqlExceptionContext._

sealed trait SqlExceptionContext {
  def trace: Trace = this match {
    case SqlExceptionContextImpl(_, t) => t
  }

  def exception: SqlException = this match {
    case SqlExceptionContextImpl(e, _) => e
  }

  // Note: O(n)
  def traceList: List[TraceOp] =
    trace.list

}
private case class SqlExceptionContextImpl(e: SqlException, t: Trace) extends SqlExceptionContext

object SqlExceptionContext extends SqlExceptionContextFunctions

trait SqlExceptionContextFunctions {
  type SqlException =
    java.sql.SQLException

  import Lens._
  import CoStateT._

  val sqlExceptionL: SqlExceptionContext @-@ SqlException =
    lens(s => coState((k => SqlExceptionContextImpl(k, s.trace), s.exception)))

  val sqlExceptionTraceL: SqlExceptionContext @-@ Trace =
    lens(s => coState((k => SqlExceptionContextImpl(s.exception, k), s.trace)))

}