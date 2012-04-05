package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class StrTest extends Spec {
  checkAll(order.laws[Str])
  checkAll(monoid.laws[Str])
}
