package scalaz
package sql


sealed trait Connection {
  val value: java.sql.Connection
}

object Connection extends ConnectionFunctions

trait ConnectionFunctions {
  def apply(c: java.sql.Connection): Connection =
    new Connection {
      val value = c
    }
}