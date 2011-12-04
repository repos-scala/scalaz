package scalaz
package sql


sealed trait Connection {
  val value: java.sql.Connection
}