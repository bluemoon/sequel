package sql.ast

import cats.Functor

import qq.droste._
import qq.droste.data._
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

  // EnvT[E, W[_], A] = (E, W[A])
  //
  // I *think* what i want is Cofree:
  //
  // Cofree[F[_], A] = (A, F[Cofree[F, A]])
  // Coalgebra[F, A, A] = GCoalgebra[F[_], A, A]

  // Coalgebra:
  //  A => F[A]
  // Algebra:
  //  F[A] => A

  // In our case we want to go
  // SqlF => EnvT[a]
  implicit val withPathFunctor: Functor[WithPath] = new Functor[WithPath] {
    override def map[A, B](fa: EnvT[String, SqlF, A])(f: A => B): EnvT[String, SqlF, B] = {
      val env = EnvT.un(fa)
      val fb = implicitly[Functor[SqlF]].map(env._2)(fa => f(fa))
      EnvT(env._1, fb)
    }
  }

  type WithPath[A] = EnvT[String, SqlF, A]
  // Match the RHS of Coalgebra
  //  F[A] becomes SqlF[WithPath[A]]
  // expanding into:
  //  SqlF[EnvT[String, SqlF, A]]
  // when you put this into a Coalgebra of EnvT[String, SqlF, A]] and (String, Mu[SqlF])  you get
  //  SqlF[EnvT[String, SqlF, (String, Mu[SqlF])]
  val coalg: Coalgebra[WithPath, (String, Mu[SqlF])] = Coalgebra {
    case (string: String, y: Mu[SqlF]) => Mu.un(y) match {
      case s: Select2[_] => EnvT("imgonna", Select2(("wowitworks", y)))
      case s: Nothing[_] => EnvT("f", Nothing())
    }
  }
  // what does it mean to go from an EnvT[..., ..., A] to EnvT[..., ..., B]
  // idk


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
