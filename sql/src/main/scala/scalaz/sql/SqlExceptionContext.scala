package scalaz
package sql

import SqlExceptionContext._

sealed trait SqlExceptionContext {

}
private case class SqlExceptionContextImpl(e: SqlException, t: Trace) extends SqlExceptionContext

object SqlExceptionContext extends SqlExceptionContextFunctions

trait SqlExceptionContextFunctions {
  type SqlException =
    java.sql.SQLException
}