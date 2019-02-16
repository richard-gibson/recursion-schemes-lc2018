package rec

import matryoshka._
import matryoshka.data._
import matryoshka.implicits._
import scalaz._
import Scalaz._

/**
  * https://medium.com/@wiemzin/getting-started-with-recursion-schemes-using-matryoshka-f5b5ec01bb
  *
 **/
object dolls {
  sealed trait Matryoshka[T]
  case class Doll[T](name: String, daughter: T) extends Matryoshka[T]
  case class Tiny[T](name: String)              extends Matryoshka[T]

  case class Person(name: String, age: Int)

//case class Fix[F[_]](unFix: F[Fix[F]])
  implicit val dollsFunctor: Functor[Matryoshka] = new Functor[Matryoshka] {
    override def map[A, B](fa: Matryoshka[A])(f: A => B): Matryoshka[B] =
      fa match {
        case Doll(n, d) => Doll(n, f(d))
        case Tiny(s)    => Tiny(s)
      }
  }

  val coalgebra: Coalgebra[Matryoshka, NonEmptyList[String]] = {
    case NonEmptyList(h, _: INil[String]) => Tiny(h)
    case NonEmptyList(h, l) =>
      val list: List[String] = l.toList
      Doll(h, NonEmptyList(list.head, list.tail: _*))
  }
  //val names = NonEmptyList("a", "b", "c", "d")
  // val result = names.ana[Fix[Matryoshka]](coalgebra)

  //def ana[A](a: A)(f: Coalgebra[F, A])(implicit F: Functor[F]): F[A]

  val algebra: Algebra[Matryoshka, Int] = {
    case Doll(_, d) => d + 1
    case Tiny(_)    => 1
  }

  // val dolls: Fix[Matryoshka] = Fix(Doll("Anna", Fix(Doll("Lisa", Fix[Matryoshka](Tiny("Kate"))))))
  // val result = dolls.cata[Int](algebra)

  val algebraPerson: Int => Algebra[Matryoshka, List[Person]] =
    base => {
      case Doll(n, d) => d :+ Person(n, d.last.age + 1)
      case Tiny(n)    => Person(n, base) :: Nil
    }
}
