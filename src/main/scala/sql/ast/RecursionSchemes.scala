package sql.ast

import cats.Functor

import qq.droste._
import qq.droste.data._
import qq.droste.data.prelude._

import qq.droste.implicits._


// We add the F because its Functor-able
sealed trait SqlF[A]

case class Nothing[A]() extends SqlF[A]
case class Select2[A](a: A) extends SqlF[A]

object RecursionSchemes {
  type Sql = Mu[SqlF]

  object dsl {
    def nothing: Mu[SqlF] ={
      Mu(Nothing())
    }

    def select(a: Mu[SqlF]): Mu[SqlF] = {
      Mu(Select2(a))
    }
  }

  // Notes:
  // Other stuff i found:
  // https://github.com/higherkindness/skeuomorph
  implicit val ff: Functor[SqlF] = new Functor[SqlF] {
    override def map[A, B](fa: SqlF[A])(f: A => B): SqlF[B] = fa match {
      case Nothing() => Nothing[B]()
      case Select2(a) => Select2(f(a))
    }
  }

  // what is a
  // - Basis
  //
  // EnvT:
  //  EnvT[ann, f, a] is isomorphic to (ann, f a)
  //
  // Cofree:
  //  Cofree[f, ann] is isomorphic to (ann, f)

  // Types for reference:
  //   EnvT[E, W[_], A] = (E, W[A])
  //   Cofree[F[_], A] = (A, F[Cofree[F, A]])
  //   Coalgebra[F, A, A] = GCoalgebra[F[_], A, A]

  // Coalgebra:
  //  A => F[A]
  // Algebra:
  //  F[A] => A

  // In our case we want to go
  // SqlF => EnvT[a]

  type WithPath[A] = EnvT[String, SqlF, A]
  // Match the RHS of Coalgebra
  //  F[A] becomes SqlF[WithPath[A]]
  // expanding into:
  //  SqlF[EnvT[String, SqlF, A]]
  // when you put this into a Coalgebra of EnvT[String, SqlF, A]] and (String, Mu[SqlF])  you get
  //  SqlF[EnvT[String, SqlF, (String, Mu[SqlF])]
  val coalg: Coalgebra[WithPath, (String, Mu[SqlF])] = Coalgebra {
    case (string: String, y: Mu[SqlF]) => EnvT("what", Select2(("", y)))

//      Mu.un(y) match {
//      case s: Select2[_] => EnvT("imgonna", Select2((string, y)))
//      case s: Nothing[_] => EnvT("f", Nothing())
//    }
  }
  // what does it mean to go from an EnvT[..., ..., A] to EnvT[..., ..., B]
  // idk




  // lets play the type game again
  // Cofree[SqlF, A] -> Cofree[SqlF, Cofree[SqlF, String]]
//  type CFH[A] = Cofree[SqlF, A]
//  val cofree: Coalgebra[CFH, Cofree[SqlF, String]] = Coalgebra {
//
//  }



  val renderString: Algebra[SqlF, String] = Algebra {
    case Nothing() => ""
  }

  def test: String = {
    val fn = scheme.ana(coalg)
    println(fn("", dsl.select(dsl.nothing)))

    val f: Mu[SqlF] => String = scheme.cata(renderString)
    f(dsl.nothing)

  }
}
