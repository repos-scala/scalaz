package scalaz
package sql

sealed trait PreparedStatement {
  val value: java.sql.PreparedStatement
}