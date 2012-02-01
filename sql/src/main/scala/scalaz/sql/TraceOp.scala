package scalaz
package sql

sealed trait TraceOp {

}
private case class StatementExecuteQuery(sql: String) extends TraceOp

object TraceOp extends TraceOpFunctions

trait TraceOpFunctions {
  def statementExecuteQuery(sql: String): TraceOp =
    StatementExecuteQuery(sql)
}