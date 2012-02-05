package scalaz
package sql

sealed trait TraceOp {

}
case class Connect(url: String) extends TraceOp
case class ConnectUserPass(url: String, user: String, pass: String) extends TraceOp
case class StatementExecuteQuery(sql: String) extends TraceOp

object TraceOp extends TraceOpFunctions

trait TraceOpFunctions