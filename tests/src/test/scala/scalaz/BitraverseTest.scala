package scalaz

import scalaz.std.AllInstances.{tuple2Instance => _, _}
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Prop.forAll

object BitraverseTest extends SpecLite {

  implicit val LE = Bitraverse[\/].leftTraverse[Int]
  implicit val RE = Bitraverse[\/].rightTraverse[Int]

  checkAll("Left-biased Bitraverse for Either",  traverse.laws[({type λ[α] = α \/ Int})#λ])
  checkAll("Right-biased Bitraverse for Either", traverse.laws[({type λ[α] = Int \/ α})#λ])


  implicit val LT = Bitraverse[Tuple2].leftTraverse[Int]
  implicit val RT = Bitraverse[Tuple2].rightTraverse[Int]

  checkAll("Left-biased Bitraverse for (,)",  traverse.laws[({type λ[α] = (α, Int)})#λ])
  checkAll("Right-biased Bitraverse for (,)", traverse.laws[({type λ[α] = (Int, α)})#λ])


  "left/right bias" in {
    import scalaz.syntax.either._

    Bitraverse[\/].rightTraverse.traverse(42.left[Int])(x => Vector(x + 3)) must_===(Vector(-\/(42)))
    Bitraverse[\/].leftTraverse.traverse(42.left[Int])(x => Vector(x + 3))  must_===(Vector(-\/(45)))

    Bifoldable[\/].leftFoldable.foldMap(42.left[Int])(identity)  must_===(42)
    Bifoldable[\/].rightFoldable.foldMap(42.left[Int])(identity) must_===(0)
  }

  "both sides, left and right embedding" in {
    implicit val L_E_LO_RL: Traverse[({type λ[α] = Option[α] \/ List[Int]})#λ] = Bitraverse[\/].embed[Option,List].leftTraverse[Int]
    implicit val R_E_LO_RL: Traverse[({type λ[α] = Option[Int] \/ List[α]})#λ] = Bitraverse[\/].embed[Option,List].rightTraverse[Int]
    checkAll("Left-biased Bitraverse for Either[Option,List[_]]", traverse.laws[({type λ[α] = Option[α] \/ List[Int]})#λ])
    checkAll("Right-biased Bitraverse for Either[Option[_],List]", traverse.laws[({type λ[α] = Option[Int] \/ List[α]})#λ])

    implicit val L_E_LO = Bitraverse[\/].embedLeft[Option].leftTraverse[Int]
    implicit val R_E_RO = Bitraverse[\/].embedRight[Option].rightTraverse[Int]
    checkAll("Left-biased Bitraverse for Either[Option,_]", traverse.laws[({type λ[α] = Option[α] \/ Int})#λ])
    checkAll("Right-biased Bitraverse for Either[_,Option]", traverse.laws[({type λ[α] = Int \/ Option[α]})#λ])
  }

}

// vim: expandtab:ts=2:sw=2
