package scalaz
package benchmark

import scala.util.control.NonFatal
import scala.reflect.ClassTag
import Ordering._
import Isomorphism.{<~>, IsoFunctorTemplate}

/** An optional value
 *
 * A `Maybe[A]` will either be a wrapped `A` instance (`Just[A]`), or a lack of underlying
 * `A` instance (`Empty[A]`).
 *
 * `Maybe[A]` is isomorphic to `Option[A]`, however there are some differences between
 * the two. `Maybe` is invariant in `A` while `Option` is covariant. `Maybe[A]` does not expose
 * an unsafe `get` operation to access the underlying `A` value (that may not exist) like
 * `Option[A]` does. `Maybe[A]` does not come with an implicit conversion to `Iterable[A]` (a
 * trait with over a dozen super types).
 */
sealed abstract class MaybeS[A] {
  import MaybeS._

  /** Catamorphism.
   * Run the given function on the underlying value if present, otherwise return
   * the provided fallback value */
  final def cata[B](f: A => B, b: => B): B =
    this match {
      case Just(a) => f(a)
      case Empty() => b
    }

  /** Return the underlying value if present, otherwise the provided fallback value */
  final def getOrElse(a: => A): A =
    cata(identity, a)

  /** alias for [[getOrElse]] */
  final def |(a: => A): A =
    getOrElse(a)

  /** Turn the underlying value into a failure validation if present, otherwise
   * return a success validation with the provided fallback value */
  final def toFailure[B](b: => B): Validation[A, B] =
    cata(Failure(_), Success(b))

  /** Turn the underlying value into a success validation if present, otherwise
   * return a failure validation with the provided fallback value */
  final def toSuccess[B](b: => B): Validation[B, A] =
    cata(Success(_), Failure(b))

  /** Turn the underlying value into a left disjunction if present, otherwise
   * return a right disjunction with the provided fallback value */
  final def toLeft[B](b: => B): A \/ B =
    cata(\/.left(_), \/.right(b))

  /** alias for [[toLeft]] */
  final def <\/[B](b: => B): A \/ B =
    toLeft(b)

  /** Turn the underlying value into a right disjunction if present, otherwise
   * return a left disjunction with the provided fallback value */
  final def toRight[B](b: => B): B \/ A =
    cata(\/.right(_), \/.left(b))

  /** alias for [[toRight]] */
  final def \/>[B](b: => B): B \/ A =
    toRight(b)

  /** True if an underlying value is present */
  final def isJust: Boolean =
    cata(_ => true, false)

  /** True if no underlying value is present */
  def isEmpty: Boolean

  final def map[B](f: A => B): MaybeS[B] =
    cata(f andThen just[B], empty[B])

  final def flatMap[B](f: A => MaybeS[B]) =
    cata(f, empty[B])

  /** Convert to a standard library `Option` */
  final def toOption: Option[A] =
    cata(Some(_), None)

  /** Return this instance if it is a [[Maybe.Just]], otherwise the provided fallback */
  final def orElse(oa: => MaybeS[A]): MaybeS[A] =
    cata(_ => this, oa)

  final def cojoin: MaybeS[MaybeS[A]] = map(just)

  final def cobind[B](f: MaybeS[A] => B): MaybeS[B] =
    map(_ => f(this))

  final def zip[B](fb: MaybeS[B]): MaybeS[(A, B)] =
    for {
      a <- this
      b <- fb
    } yield (a, b)

  final def zipWith[B, C](fb: MaybeS[B])(f: (A, B) => C): MaybeS[C] =
    for {
      a <- this
      b <- fb
    } yield f(a, b)

  final def filter(f: A => Boolean): MaybeS[A] =
    flatMap(a => if (f(a)) this else empty)

  final def filterNot(f: A => Boolean): MaybeS[A] =
    filter(f.andThen(!_))

  /** Return `true` if this is a [[Maybe.Empty]] or if this is a [[Maybe.Just]]
   * and the underlying value satisfies the provided predicate */
  final def forall(f: A => Boolean): Boolean =
    cata(f, true)

  /** Return `true` if this is a [[Maybe.Just]] and the underlying value
   * satisfies the provided predicate */
  final def exists(f: A => Boolean): Boolean =
    cata(f, false)

  /** Return the underlying value if present, otherwise the monoid zero */
  final def orZero(implicit F: Monoid[A]): A =
    getOrElse(F.zero)

  /** alias for [[orZero]] */
  final def unary_~(implicit z: Monoid[A]): A =
    orZero

  /**
   * Return the underlying value wrapped in type `F` if present, otherwise the
   * empty value for type `F` */
  final def orEmpty[F[_]](implicit F: Applicative[F], G: PlusEmpty[F]): F[A] =
    cata(F.point(_), G.empty)
}

object MaybeS extends MaybeSInstances with MaybeSFunctions {

  case object Empty extends MaybeS[Nothing] {
    override val isEmpty = true
    def unapply[A](m: MaybeS[A]) = m.isEmpty
    def apply[A](): MaybeS[A] = this.asInstanceOf[MaybeS[A]] // YOLO
    
  }

  final case class Just[A](a: A) extends MaybeS[A] {
    override def isEmpty = false
  }

  val optionMaybeIso: Option <~> MaybeS =
    new IsoFunctorTemplate[Option, MaybeS] {
      def to[A](fa: Option[A]) = fa.fold[MaybeS[A]](Empty())(Just.apply)
      def from[A](ga: MaybeS[A]) = ga.toOption
    }
}

sealed trait MaybeSFunctions {
  import MaybeS._

  final def empty[A]: MaybeS[A] = Empty()

  final def just[A](a: A): MaybeS[A] = Just(a)

  final def fromOption[A](oa: Option[A]): MaybeS[A] =
    std.option.cata(oa)(just, empty)

  def fromTryCatchThrowable[T, E <: Throwable](a: => T)(implicit nn: NotNothing[E], ex: ClassTag[E]): MaybeS[T] = try {
    just(a)
  } catch {
    case e if ex.runtimeClass.isInstance(e) => empty
  }

  def fromTryCatchNonFatal[T](a: => T): MaybeS[T] = try {
    just(a)
  } catch {
    case NonFatal(t) => empty
  }
}

sealed abstract class MaybeSInstances {
  import MaybeS._

  implicit def maybeEqual[A : Equal]: Equal[MaybeS[A]] = new MaybeSEqual[A] {
    def A = implicitly
  }

  implicit def maybeOrder[A : Order]: Order[MaybeS[A]] = new Order[MaybeS[A]] with MaybeSEqual[A] {
    def A = implicitly

    def order(fa1: MaybeS[A], fa2: MaybeS[A]) =
      fa1.cata(
        a1 => fa2.cata(
          a2 => Order[A].order(a1, a2),
          GT),
        fa2.cata(_ => LT, EQ))
  }

  implicit def maybeShow[A](implicit A: Show[A]): Show[MaybeS[A]] =
    Show.show(_.cata(
      a => Cord("Just(", A.show(a), ")"),
      "Empty"))

  implicit def maybeMonoid[A](implicit A: Semigroup[A]): Monoid[MaybeS[A]] = new Monoid[MaybeS[A]] {
    def append(fa1: MaybeS[A], fa2: => MaybeS[A]) =
      fa1.cata(
        a1 => fa2.cata(a2 => just(A.append(a1, a2)), fa1),
        fa2.cata(_ => fa2, empty))

    def zero = empty
  }

  implicit val maybeInstance = new Traverse[MaybeS] with MonadPlus[MaybeS] with Cozip[MaybeS] with Zip[MaybeS] with Unzip[MaybeS] with Align[MaybeS] with IsEmpty[MaybeS] with Cobind[MaybeS] with Optional[MaybeS] {

    def point[A](a: => A) = just(a)

    override def ap[A, B](fa: => MaybeS[A])(mf: => MaybeS[A => B]) =
      mf.cata(f => fa.cata(f andThen just, empty), empty)

    def bind[A, B](fa: MaybeS[A])(f: A => MaybeS[B]) = fa flatMap f

    override def map[A, B](fa: MaybeS[A])(f: A => B) = fa map f

    def traverseImpl[F[_], A, B](fa: MaybeS[A])(f: A => F[B])(implicit F: Applicative[F]) =
      fa.cata(a => F.map(f(a))(just), F.point(empty))

    def empty[A]: MaybeS[A] = MaybeS.empty

    def plus[A](a: MaybeS[A], b: => MaybeS[A]) = a orElse b

    override def foldRight[A, B](fa: MaybeS[A], z: => B)(f: (A, => B) => B) =
      fa.cata(f(_, z), z)

    def cozip[A, B](fa: MaybeS[A \/ B]) =
      fa.cata(_.leftMap(just).map(just), \/.left(empty))

    def zip[A, B](a: => MaybeS[A], b: => MaybeS[B]) = a.zip(b)

    def unzip[A, B](a: MaybeS[(A, B)]) =
      a.cata(ab => (just(ab._1), just(ab._2)), (empty, empty))

    def alignWith[A, B, C](f: A \&/ B => C) = (fa, fb) =>
      fa.cata(
        a => fb.cata(
          b => just(f(\&/.Both(a, b))),
          just(f(\&/.This(a)))),
        fb.cata(
          b => just(f(\&/.That(b))),
          empty))

    def cobind[A, B](fa: MaybeS[A])(f: MaybeS[A] => B) =
      fa.cobind(f)

    override def cojoin[A](a: MaybeS[A]) =
      a.cojoin

    def pextract[B, A](fa: MaybeS[A]): MaybeS[B] \/ A =
      fa.cata(\/.right, \/.left(empty))

    override def isDefined[A](fa: MaybeS[A]): Boolean = fa.isJust

    override def toOption[A](fa: MaybeS[A]): Option[A] = fa.toOption

    override def filter[A](fa: MaybeS[A])(f: A => Boolean): MaybeS[A] =
      fa.filter(f)
  }
}

private sealed trait MaybeSEqual[A] extends Equal[MaybeS[A]] {
  implicit def A: Equal[A]

  override final def equal(fa1: MaybeS[A], fa2: MaybeS[A]) =
    fa1.cata(
      a1 => fa2.cata(a2 => A.equal(a1, a2), false),
      fa2.cata(_ => false, true))
}
