package scalaz

sealed trait BijectionT[A, F[_], B] {
  def to(a: A): F[B]
  def fr(b: B): A

  import BijectionT._

  def toK: Kleisli[F, A, B] =
    Kleisli(to(_))

  def bimap[C, X[_, _], D](g: Bijection[C, D])(implicit F: BiFunctor[X], ev: F[B] =:= Id[B]): Bijection[X[A, C], X[B, D]] =
    bijection(
      F.bimap(_)(to(_), g.to(_)): Id[X[B, D]]
    , F.bimap(_)(fr(_), g.fr(_))
    )

  def compose[C](g: BijectionT[C, F, A])(implicit M: Bind[F]): BijectionT[C, F, B] =
    bijection(
      (toK <=< g.toK) run _
    , x => (g.fr(fr(x)))
    )

  /** alias for `compose` */
  def >=>[C](that: BijectionT[C, F, A])(implicit M: Bind[F]): BijectionT[C, F, B] = compose(that)

  def andThen[C](that: BijectionT[B, F, C])(implicit M: Bind[F]): BijectionT[A, F, C] =
    that compose this

  /** alias for `andThen` */
  def <=<[C](that: BijectionT[B, F, C])(implicit M: Bind[F]): BijectionT[A, F, C] = andThen(that)

}
object BijectionT extends BijectionTFunctions with BijectionTInstances

trait BijectionTFunctions {
  def bijection[A, F[_], B](t: A => F[B], f: B => A): BijectionT[A, F, B] =
    new BijectionT[A, F, B] {
      def to(a: A) = t(a)
      def fr(b: B) = f(b)
    }

  type Bijection[A, B] =
  BijectionT[A, Id, B]
}

trait BijectionTInstances {
}