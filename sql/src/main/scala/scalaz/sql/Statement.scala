package scalaz
package sql

sealed trait Statement {
  val value: java.sql.PreparedStatement
}