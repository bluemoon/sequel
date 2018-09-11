package sql.ast

import cats.Functor

import qq.droste._
import qq.droste.data._
import qq.droste.syntax.all._
import qq.droste.data.prelude._
import cats.implicits._

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
  implicit val fff: Functor[SqlF] = new Functor[SqlF] {
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

//  type WithPath[A] = EnvT[String, SqlF, ?]
  // Match the RHS of Coalgebra
  //  F[A] becomes SqlF[WithPath[A]]
  // expanding into:
  //  SqlF[EnvT[String, SqlF, A]]
  // when you put this into a Coalgebra of EnvT[String, SqlF, A]] and (String, Mu[SqlF])  you get
  //  SqlF[EnvT[String, SqlF, (String, Mu[SqlF])]
  // what does it mean to go from an EnvT[..., ..., A] to EnvT[..., ..., B]
  // idk

  def attributeTopDown[F[_]: Functor, A](f: (A, Mu[F]) => A): (A, Mu[F]) => Cofree[F, A] = {
    Function.untupled(scheme.ana(Coalgebra[EnvT[A, F, ?], (A, Mu[F])] {
      case (a: A, ff: Mu[F]) =>
        EnvT(a,  Mu.un(ff).map(z => f(a, ff) -> z))
    }))
  }

  val renderString: Algebra[SqlF, String] = Algebra {
    case Nothing() => ""
  }

  def depth[F[_]]: (Int, Mu[F]) => Int = (z, _) => z + 1

  def pfDepth: (Int, Mu[SqlF]) => Int = (z, y) => {
    val m: SqlF[Mu[SqlF]] = Mu.un(y)
    m match {
      case s: Select2[_] => z + 2
      case _ => z
    }
  }

  def f: (Int, Mu[SqlF]) => Cofree[SqlF, Int] = attributeTopDown(depth[SqlF])
  def f2: (Int, Mu[SqlF]) => Cofree[SqlF, Int] = attributeTopDown(pfDepth)

  def test: String = {
    println(f(0, dsl.select(dsl.nothing)))
    println(f2(1, dsl.select(dsl.nothing)))

    val fz: Mu[SqlF] => String = scheme.cata(renderString)
    fz(dsl.nothing)

  }
}
