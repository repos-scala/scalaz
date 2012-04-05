package scalaz

import Lens._
import PLens._
import DList._
import std.AllInstances._
import collection.immutable.StringOps

/** A string value that can be constructed with a regular `String` or a list type.
 * Complexity of operations are then determined by the method of construction. There is a bias toward checking for `DList[Char]` construction first.
 * This data type is motivated solely on the basis of the syntactic aesthetics of string literals and their unfortunate representation as a data structure.
 * This data type is not intended to abstract away operations that are common to `String`, `DList[Char]`, etc. -- use type-classes for that.
 * The underlying representation of `Str` is either a `String` or a `DList[Char]`.
 */
sealed trait Str {
  private[scalaz] val v: Either[DList[Char], String]

  import Str._
  import CostateT._

  /** Switch the underlying representation from `String` to `DList[Char]` or vice versa. */
  def unary_~ : Str =
    v match {
      case Left(x)  => x.toList.mkString
      case Right(y) => y.toList
    }

  /** Force the underlying representation to `DList[Char]`. */
  def unary_+ : Str =
    v match {
      case Left(x)  => x
      case Right(y) => y.toList
    }

  /** Force the underlying representation to `String`. */
  def unary_- : Str =
    v match {
      case Left(x)  => x.toList.mkString
      case Right(y) => y
    }

  /** Prepend an element. O(1) for `DList[Char]`; O(n) for `String`. */
  def +:(c: Char): Str =
    v match {
      case Left(x)  => c +: x
      case Right(y) => c + y
    }

  /** Append an element. O(1) for `DList[Char]`; O(n) for `String`. */
  def :+(c: Char): Str =
    v match {
      case Left(x)  => c +: x
      case Right(y) => y + c
    }

  /** Append a `Str` with `DList`-bias. If either this or the argument are `DList`-constructed, the result is `DList`-constructed. */
  def ++(s: Str): Str =
    v match {
      case Left(x)  => x ++ (s.v match {
        case Left(t)  => x ++ t
        case Right(u) => x ++ DL(_ => u.toList)
      })
      case Right(y) => (s.v match {
        case Left(t)  => DL[Char](_ => y.toList) ++ t
        case Right(u) => y + u
      })
    }

  def head: Option[Costate[Char, Str]] =
    strHeadL run this

  def tail: Option[Costate[List[Char], Str]] =
    strTailL run this

  def zipper: Option[StrZipper] =
    (v match {
      case Left(x)  =>
        x.toList
      case Right(y) =>
        y.toList
    }) match {
      case Nil => None
      case h::t => Some(StrZipper(Nil, h, t))
    }

}

object Str extends StrFunctions with StrInstances

trait StrInstances {
  implicit def strInstances: Order[Str] with Show[Str] with Monoid[Str] = new Order[Str] with Show[Str] with Monoid[Str] {
    def show(s: Str) =
      s.v match {
        case Left(x)  => x.toList
        case Right(y) => y.toList
      }
    override def equal(s1: Str, s2: Str) =
      s1.v match {
        case Left(x)  =>
          s2.v match {
            case Left(t) =>
              Equal[DList[Char]].equal(x, t)
            case Right(u) =>
              Equal[String].equal(x.toList.mkString, u)
          }
        case Right(y) =>
          s2.v match {
            case Left(t) =>
              Equal[String].equal(y, t.toList.mkString)
            case Right(u) =>
              y == u
          }
      }
    def order(s1: Str, s2: Str) =
      s1.v match {
        case Left(x)  =>
          s2.v match {
            case Left(t) =>
              Order[String].order(x.toList.mkString, t.toList.mkString)
            case Right(u) =>
              Order[String].order(x.toList.mkString, u)
          }
        case Right(y) =>
          s2.v match {
            case Left(t) =>
              Order[String].order(y, t.toList.mkString)
            case Right(u) =>
              Order[String].order(y, u)
          }
      }

    def zero = DList[Char]()
    def append(s1: Str, s2: => Str) =
      s1 ++ s2
  }

}

trait StrFunctions {
  implicit def StringStr(s: String): Str =
    new Str {
      val v = Right(s)
    }

  implicit def CharDListStr(s: DList[Char]): Str =
    new Str {
      val v = Left(s)
    }

  implicit def CharListStr(s: List[Char]): Str =
    new Str {
      val v = Left(DL[Char](_ => s))
    }

  import CostateT._

  def strHeadL: Str @-? Char =
    PLens(_.v match {
      case Left(x)  =>
        x.toList match {
          case Nil => None
          case h::t => Some(costate(x => DL[Char](_ => x::t), h))
        }
      case Right(y) =>
        if(y.isEmpty)
          None
        else
          Some(costate(c => {
            val a = y.toCharArray
            a(0) = c
            new String(a)
          }, (y: StringOps)(0)))
    })

  def strTailL: Str @-? List[Char] =
    PLens(_.v match {
      case Left(x)  =>
        x.toList match {
          case Nil => None
          case h::t => Some(costate(x =>
            DL[Char](_ => h::x)
          , t))
        }
      case Right(y) =>
        if(y.isEmpty)
          None
        else
          Some(costate(c =>
            DL[Char](_ => (y: StringOps)(0) :: c)
          , y substring 1 toList))
    })

}

sealed trait StrZipper {
  val lefts: List[Char]
  val focus: Char
  val rights: List[Char]

  import CostateT._
  import StrZipper._

  def left: Option[Costate[Char, StrZipper]] =
    strZipperLeftL run this

  def right: Option[Costate[Char, StrZipper]] =
    strZipperRightL run this

  def apply(c: Char): StrZipper =
    StrZipper(lefts, c, rights)

  def toStr: Str =
    lefts.reverse ::: focus :: rights

  def <--(n: Int): Option[Costate[Char, StrZipper]] =
    strZipperNthLeftL(n) run this

  def -->(n: Int): Option[Costate[Char, StrZipper]] =
    strZipperNthRightL(n) run this
}

object StrZipper extends StrZipperFunctions {
  def apply(l: List[Char], f: Char, r: List[Char]): StrZipper =
    strZipper(l, f, r)
}

trait StrZipperFunctions {
  def strZipper(l: List[Char], f: Char, r: List[Char]): StrZipper =
    new StrZipper {
      val lefts = l
      val focus = f
      val rights = r
    }

  import CostateT._

  def strZipperLeftsL: StrZipper @-@ List[Char] =
    Lens(w => costate(x => StrZipper(x, w.focus, w.rights), w.lefts))

  def strZipperFocusL: StrZipper @-@ Char =
    Lens(w => costate(x => StrZipper(w.lefts, x, w.rights), w.focus))

  def strZipperRightsL: StrZipper @-@ List[Char] =
    Lens(w => costate(x => StrZipper(w.lefts, w.focus, x), w.rights))

  def strZipperLeftL: StrZipper @-? Char =
    PLens.listHeadPLens >=> ~strZipperLeftsL

  def strZipperRightL: StrZipper @-? Char =
    PLens.listHeadPLens >=> ~strZipperRightsL

  def strZipperNthLeftL(n: Int): StrZipper @-? Char =
    PLens.listNthPLens(n) >=> ~strZipperLeftsL

  def strZipperNthRightL(n: Int): StrZipper @-? Char =
    PLens.listNthPLens(n) >=> ~strZipperRightsL

}