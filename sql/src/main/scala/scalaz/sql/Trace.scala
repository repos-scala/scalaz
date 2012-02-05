package scalaz
package sql

import Trace._

trait Trace {
  private[sql] val value: TrO = this match {
    case TraceImpl(x) => x
  }

  // O(n)
  lazy val list: List[TraceOp] =
    value.toList

  def isEmpty: Boolean =
    list.isEmpty

  def +:(a: TraceOp): Trace =
    TraceImpl(a +: value)

  def :+(a: TraceOp): Trace =
    TraceImpl(value :+ a)

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

  def empty: Trace =
    TraceImpl(DList[TraceOp]())

  def single(o: TraceOp): Trace =
    empty :+ o

  implicit val TraceMonoid: Monoid[Trace] = new Monoid[Trace] {
    val zero = empty
    def append(a: Trace, b: => Trace) = a ++ b
 }

}