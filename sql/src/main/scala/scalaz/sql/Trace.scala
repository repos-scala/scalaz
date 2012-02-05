package scalaz
package sql

import Trace._

trait Trace {
  private[sql] val value: TrO = this match {
    case TraceImpl(x) => x
  }

  def ++(t: Trace): Trace =
    TraceImpl(value ++ t.value)
}
private case class TraceImpl(x: TrO) extends Trace

object Trace extends TraceFunctions

trait TraceFunctions {
  type Tr[A] =
  DList[A]

  type TrO =
    Tr[TraceOp]

  implicit val TraceMonoid: Monoid[Trace] = new Monoid[Trace] {
    val zero = TraceImpl(DList[TraceOp]())
    def append(a: Trace, b: => Trace) = a ++ b
 }

}