package scalaz
package benchmark

import org.scalameter.api._
import scalaz._
import Scalaz._

trait Traversing extends PerformanceTest {

  def executor = new org.scalameter.execution.LocalExecutor(
    Warmer.Default(),
    Aggregator.min,
    measurer
  )
    def measurer: org.scalameter.Measurer = new Measurer.Default
    def reporter: org.scalameter.Reporter = new org.scalameter.reporting.LoggingReporter
    def persistor: Persistor = Persistor.None

  sealed trait Optional[O[_]] {
    def cata[A,B](notThere: B, there: A ⇒ B)(fa: O[A]): B
    def there[A](a: A): O[A]
    def notThere[A]: O[A]
  }

  sealed trait Traversury {
    type O[A]
    type F[A]
    implicit val OA: Applicative[O]
    val fa: F[A]
    val T: Traverse[F]
    val O: Optional[O]
    type A
    def f: A ⇒ Boolean

    def sprinkle[A](f: A⇒Boolean)(fa: F[A]): F[O[A]] =
      T.map(fa)(a ⇒ if(f(a)) O.there(a) else O.notThere)

    def sequence: O[F[A]] = T.sequence(sprinkle(f)(fa))
  }

  val optionOptional = new Optional[Option] {
    def cata[A,B](notThere: B, there: A ⇒ B)(fa: Option[A]): B =
      fa.fold(notThere)(there)
    def there[A](a: A): Option[A] = Some(a)
    def notThere[A]: Option[A] = None
  }

  val maybeOptional = new Optional[Maybe] {
    def cata[A,B](notThere: B, there: A ⇒ B)(fa: Maybe[A]): B =
      fa.cata(there, notThere)
    def there[A](a: A): Maybe[A] = Maybe.Just(a)
    def notThere[A]: Maybe[A] = Maybe.Empty()
  }

  val theTraversers = Gen.enumeration("traversury")(new Traversury {
                                                      override def toString = "List[Option]"
                                                      type O[A] = Option[A]
                                                      type F[A] = List[A]
                                                      val T = Traverse[List]
                                                      val O = optionOptional
                                                      implicit val OA: Applicative[Option] = scalaz.std.option.optionInstance
                                                      val fa: List[Int] = (1 to 1000000).toList

                                                      type A = Int
                                                      def f = (_ % 2 == 0)
                                                    },
new Traversury {
                                                      override def toString = "List[Maybe]"
                                                      type O[A] = Maybe[A]
                                                      type F[A] = List[A]
                                                      val T = Traverse[List]
                                                      val O = maybeOptional
                                                      implicit val OA = Applicative[Maybe]
                                                      val fa: List[Int] = (1 to 1000000).toList
                                                      type A = Int
                                                      def f = (_ % 2 == 0)
                                                    })
}
object PerfTraversing extends Traversing {
  // this shit doesn't work
  val opts = Context(verbose → false)

  performance of "traversury speed test" config (opts) in {
    measure method "sequence" in {
      using(theTraversers) in { tr ⇒
        tr.sequence
      }
    }
  }
}

object MemoryTraversing extends Traversing {

  override def measurer = new Executor.Measurer.MemoryFootprint

  performance of "traversury mem footprint" in {
    measure method "sequence" in {
      using(theTraversers) in { tr ⇒
        tr.sequence
      }
    }
  }
}
