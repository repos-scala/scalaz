package scalaz
package sql

object DriverManager extends DriverManagerFunctions

trait DriverManagerFunctions {
  type Username =
  String

  type Password =
  String

  import SqlT._

  def connection(url: String, userpass: Option[(Username, Password)]): SqlIO[Connection] =
    userpass match {
      case Some((u, p)) =>
        trySqlT(ConnectUserPass(url, u, p), Connection(java.sql.DriverManager.getConnection(url, u, p)))
      case None =>
        trySqlT(Connect(url), Connection(java.sql.DriverManager.getConnection(url)))
    }

  def connect(url: String): SqlIO[Connection] =
    connection(url, None)

  def connectUserPass(url: String, user: Username, pass: Password): SqlIO[Connection] =
    connection(url, Some((user, pass)))
}