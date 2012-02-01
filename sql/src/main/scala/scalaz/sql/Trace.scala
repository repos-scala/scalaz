package scalaz
package sql

import Trace._

trait Trace {

}
private case class TraceImpl(x: Tr[TraceOp]) extends Trace

object Trace extends TraceFunctions

trait TraceFunctions {
  type Tr[A] =
  DList[A]
}