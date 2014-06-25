package scalaz
package benchmark

import org.scalameter.api._

object Listies extends PerformanceTest.Quickbenchmark {
  sealed trait ListFunctions {
    type F[A]
    type A
    val fa: F[A]
    def filter(f: A⇒Boolean): F[A]
    def reverse: F[A]
    def collect[B](pf: PartialFunction[A, B]): F[B]
    def foldLeft[B](f: (B,A) ⇒ B, b: B): B
    def foldRight[B](f: (A,B) ⇒ B, b: B): B
    def drop(n: Int): F[A]
    def take(n: Int): F[A]
    def find(f: A⇒Boolean): Option[A]
    def map[B](f: A⇒B): F[B]
    def flatMap[B](f: A⇒F[B]): F[B]
    def point(a: A): F[A]
  }

  val theListing = Gen.enumeration("listing")(new ListFunctions {
                                                override def toString = "List[Int]"
                                                type F[A] = List[A]
                                                type A = Int
                                                val fa: List[Int] = (1 to 1000000).toList
                                                override def filter(f: A⇒Boolean) = fa.filter(f)
                                                override def collect[B](pf: PartialFunction[A, B]) = fa.collect(pf)
                                                override def reverse = fa.reverse
                                                override def foldLeft[B](f: (B,A) ⇒ B, b: B) = fa.foldLeft(b)(f)
                                                override def foldRight[B](f: (A,B) ⇒ B, b: B) = fa.foldRight(b)(f)
                                                override def drop(n: Int) = fa.drop(n)
                                                override def take(n: Int) = fa.take(n)
                                                override def find(f: A⇒Boolean) = fa.find(f)
                                                override def map[B](f: A⇒B) = fa.map(f)
                                                override def flatMap[B](f: A⇒F[B]) = fa.flatMap(f)
                                                override def point(a: A) = List(a)
                                              },
                                              new ListFunctions {
                                                override def toString = "IList[Int]"
                                                type F[A] = IList[A]
                                                type A = Int
                                                val fa: IList[Int] = IList((1 to 1000000): _*)
                                                override def filter(f: A⇒Boolean) = fa.filter(f)
                                                override def collect[B](pf: PartialFunction[A, B]) = fa.collect(pf)
                                                override def reverse = fa.reverse
                                                override def foldLeft[B](f: (B,A) ⇒ B, b: B) = fa.foldLeft(b)(f)
                                                override def foldRight[B](f: (A,B) ⇒ B, b: B) = fa.foldRight(b)(f)
                                                override def drop(n: Int) = fa.drop(n)
                                                override def take(n: Int) = fa.take(n)
                                                override def find(f: A⇒Boolean) = fa.find(f)
                                                override def map[B](f: A⇒B) = fa.map(f)
                                                override def flatMap[B](f: A⇒F[B]) = fa.flatMap(f)
                                                override def point(a: A) = IList(a)
                                              })

  performance of "list things" in {

    measure method "filter" in {
      using(theListing) in { li ⇒
        li.filter(_ % 2 == 0)
        li.filter(_ % 2 == 0)
        li.filter(_ % 2 == 0)
        li.filter(_ % 2 == 0)
      }
    }
    measure method "collect" in {
      using(theListing) in { li ⇒
        li.collect{ case x if x % 2 == 0 ⇒ x.toString }
        li.collect{ case x if x % 2 == 0 ⇒ x.toString }
      }
    }
    measure method "reverse" in {
      using(theListing) in { li ⇒
        li.reverse
        li.reverse
        li.reverse
        li.reverse
        li.reverse
      }
    }
    measure method "foldLeft" in {
      using(theListing) in { li ⇒
        li.foldLeft[Int]((_+_), 0)
        li.foldLeft[Int]((_+_), 0)
        li.foldLeft[Int]((_+_), 0)
        li.foldLeft[Int]((_+_), 0)
      }
    }
    measure method "foldRight" in {
      using(theListing) in { li ⇒
        li.foldRight[Int]((_+_), 0)
        li.foldRight[Int]((_+_), 0)
        li.foldRight[Int]((_+_), 0)
        li.foldRight[Int]((_+_), 0)
      }
    }
    measure method "drop" in {
      using(theListing) in { li ⇒
        li.drop(500000)
        li.drop(500000)
        li.drop(500000)
        li.drop(500000)
        li.drop(500000)
        li.drop(500000)
        li.drop(500000)
        li.drop(500000)
      }
    }
    measure method "take" in {
      using(theListing) in { li ⇒
        li.take(500000)
        li.take(500000)
        li.take(500000)
        li.take(500000)
        li.take(500000)
        li.take(500000)
        li.take(500000)
        li.take(500000)
      }
    }
    measure method "find" in {
      using(theListing) in { li ⇒
        li.find(_ == 500000)
        li.find(_ == 500000)
        li.find(_ == 500000)
        li.find(_ == 500000)
        li.find(_ == 500000)
        li.find(_ == 500000)
        li.find(_ == 500000)
        li.find(_ == 500000)
        li.find(_ == 500000)
        li.find(_ == 500000)
      }
    }
    measure method "map" in {
      using(theListing) in { li ⇒
        li.map(_.toString)
        li.map(_.toString)
        li.map(_.toString)
      }
    }
    measure method "flatMap" in {
      using(theListing) in { li ⇒
        li.flatMap[Int](li.point)
        li.flatMap[Int](li.point)
      }
    }
  }
}
