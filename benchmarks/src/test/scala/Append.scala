package scalaz
package benchmark

import std.list._
import std.vector._
import std.anyVal._
import std.map._
import std.string._

import org.scalameter.api._

object MonoidShootout extends PerformanceTest.Quickbenchmark {

  sealed trait Spacedust {
    type T[A]
    type A
    def mon: Monoid[T[A]]
    def fold: Foldable[T]
    def point(a: A): T[A]
    def fromSeq(a: A*): T[A]
  }

  val spacedust = Gen.enumeration("spacedust")(new Spacedust {
                                                 type T[A] = IList[A]
                                                 type A = Int
                                                 def mon = implicitly[Monoid[IList[Int]]]
                                                 def fold = implicitly[Foldable[IList]]
                                                 override def toString = "IList[Int]"
                                                 def point(a: Int) = IList(a)
                                                 def fromSeq(a: Int*) = IList(a: _*)
                                               },
                                               new Spacedust {
                                                 type T[A] = List[A]
                                                 type A = Int
                                                 def mon = implicitly[Monoid[List[Int]]]
                                                 def fold = implicitly[Foldable[List]]
                                                 override def toString = "List[Int]"
                                                 def point(a: Int) = List(a)
                                                 def fromSeq(a: Int*) = List(a: _*)
                                               },
                                               new Spacedust {
                                                 type T[A] = DList[A]
                                                 type A = Int
                                                 def mon = implicitly[Monoid[DList[Int]]]
                                                 def fold = implicitly[Foldable[DList]]
                                                 override def toString = "DList[Int]"
                                                 def point(a: Int) = DList(a)
                                                 def fromSeq(a: Int*) = DList(a: _*)
                                               },
                                               new Spacedust {
                                                 type T[A] = Vector[A]
                                                 type A = Int
                                                 def mon = implicitly[Monoid[Vector[Int]]]
                                                 def fold = implicitly[Foldable[Vector]]
                                                 override def toString = "Vector[Int]"
                                                 def point(a: Int) = Vector(a)
                                                 def fromSeq(a: Int*) = Vector(a: _*)
                                               },
                                               new Spacedust {
                                                 type T[A] = Map[String,A]
                                                 type A = Int
                                                 def mon = implicitly[Monoid[Map[String, Int]]]
                                                 def fold = implicitly[Foldable[T]]
                                                 override def toString = "Map[String, Int]"
                                                 def point(a: Int) = Map(a.toString →a)
                                                 def fromSeq(a: Int*) = Map((a.map(a ⇒ a.toString→a)): _*)
                                               },
                                               new Spacedust {
                                                 type T[A] = String ==>> A
                                                 type A = Int
                                                 def mon = implicitly[Monoid[String ==>> Int]]
                                                 def fold = implicitly[Foldable[T]]
                                                 override def toString = "String ==>> Int"
                                                 def point(a: Int) = ==>>[String,Int](a.toString → a)
                                                 def fromSeq(a: Int*) = ==>>((a.map(a ⇒ a.toString→a)): _*)
                                               })
                                               

  performance of "monoid appends" in {
    measure method "append" in {
      using(spacedust) in { sd ⇒
        var fa = sd.mon.zero
        for(n ← 1 to 200000) {
          sd.mon.append(fa,sd.point(n))
        }
      }
    }
    measure method "prepend" in {
      using(spacedust) in { sd ⇒
        var fa = sd.mon.zero
        for(n ← 1 to 200000) {
          sd.mon.append(sd.point(n),fa)
        }
      }
    }
    measure method "create from list" in {

      val x = (1 to 20000).toList
      using(spacedust) in { sd ⇒
        for(n ← 1 to 200) {
          sd.fromSeq(x: _*)
        }
      }
    }
    measure method "append two big lists" in {
      val x = (1 to 20000).toList
      using(spacedust) in { sd ⇒
        val x2 = sd.fromSeq(x: _*)
        for(n ← 1 to 200) {
          sd.mon.append(x2, x2)
        }
      }
    }
  }
}
