package scalaz
package benchmark

import std.list._
import std.vector._
import std.anyVal._
import std.map._
import std.set._
import std.string._

import org.scalameter.api._

object MonoidShootout extends PerformanceTest.Quickbenchmark {

  sealed trait Spacedust {
    type T[A]
    type A
    def mon: Monoid[T[A]]
    def fold: Foldable[T]
    def point(a: A): T[A]
    def fromSeq(a: Vector[A]): T[A]
  }

  val spacedust = Gen.enumeration("spacedust")(new Spacedust {
                                                 type T[A] = IList[A]
                                                 type A = Int
                                                 def mon = implicitly[Monoid[IList[Int]]]
                                                 def fold = implicitly[Foldable[IList]]
                                                 override def toString = "IList[Int]"
                                                 def point(a: Int) = IList(a)
                                                 def fromSeq(a: Vector[A]) = IList(a: _*)
                                               },
                                               new Spacedust {
                                                 type T[A] = List[A]
                                                 type A = Int
                                                 def mon = implicitly[Monoid[List[Int]]]
                                                 def fold = implicitly[Foldable[List]]
                                                 override def toString = "List[Int]"
                                                 def point(a: Int) = List(a)
                                                 def fromSeq(a: Vector[A]) = List(a: _*)
                                               },
                                               new Spacedust {
                                                 type T[A] = DList[A]
                                                 type A = Int
                                                 def mon = implicitly[Monoid[DList[Int]]]
                                                 def fold = implicitly[Foldable[DList]]
                                                 override def toString = "DList[Int]"
                                                 def point(a: Int) = DList(a)
                                                 def fromSeq(a: Vector[A]) = DList(a: _*)
                                               },
                                               new Spacedust {
                                                 type T[A] = Vector[A]
                                                 type A = Int
                                                 def mon = implicitly[Monoid[Vector[Int]]]
                                                 def fold = implicitly[Foldable[Vector]]
                                                 override def toString = "Vector[Int]"
                                                 def point(a: Int) = Vector(a)
                                                 def fromSeq(a: Vector[A]) = Vector(a: _*)
                                               },
                                               new Spacedust {
                                                 type T[A] = Map[String,A]
                                                 type A = Int
                                                 def mon = implicitly[Monoid[Map[String, Int]]]
                                                 def fold = implicitly[Foldable[T]]
                                                 override def toString = "Map[String, Int]"
                                                 def point(a: Int) = Map(a.toString →a)
                                                 def fromSeq(a: Vector[A]) = Map((a.map(a ⇒ a.toString→a)): _*)
                                               },
                                               new Spacedust {
                                                 type T[A] = String ==>> A
                                                 type A = Int
                                                 def mon = implicitly[Monoid[String ==>> Int]]
                                                 def fold = implicitly[Foldable[T]]
                                                 override def toString = "String ==>> Int"
                                                 def point(a: Int) = ==>>[String,Int](a.toString → a)
                                                 def fromSeq(a: Vector[A]) = ==>>((a.map(a ⇒ a.toString→a)): _*)
                                               },
                                               new Spacedust {
                                                 type T[A] = ISet[A]
                                                 type A = Int
                                                 def mon = implicitly[Monoid[ISet[A]]]
                                                 def fold = implicitly[Foldable[ISet]]
                                                 override def toString = "ISet[Int]"
                                                 def point(a: Int) = ISet.singleton(a)
                                                 def fromSeq(a: Vector[A]) = ISet.fromFoldable(a)
                                               },
                                               new Spacedust {
                                                 type T[A] = Set[A]
                                                 type A = Int
                                                 def mon = implicitly[Monoid[Set[A]]]
                                                 def fold = implicitly[Foldable[Set]]
                                                 override def toString = "Set[Int]"
                                                 def point(a: Int) = Set(a)
                                                 def fromSeq(a: Vector[A]) = Set(a: _*)
                                               }
                                               )
                                               

  performance of "monoid appends" in {
    measure method "append" in {
      using(spacedust) in { sd ⇒
        var fa = sd.mon.zero
        for(n ← 1 to 200000) {
          sd.mon.append(fa,sd.point(n))
        }
      }
    }
    measure method "monoid prepend" in {
      using(spacedust) in { sd ⇒
        var fa = sd.mon.zero
        for(n ← 1 to 200000) {
          sd.mon.append(sd.point(n),fa)
        }
      }
    }
    measure method "create from list" in {

      val x = (1 to 20000).toVector
      using(spacedust) in { sd ⇒
        for(n ← 1 to 200) {
          sd.fromSeq(x)
        }
      }
    }
    measure method "append two big lists" in {
      val x = (1 to 20000).toVector
      using(spacedust) in { sd ⇒
        val x2 = sd.fromSeq(x)
        for(n ← 1 to 200) {
          sd.mon.append(x2, x2)
        }
      }
    }
  }
}
