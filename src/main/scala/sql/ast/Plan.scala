package sql.ast

import matryoshka._
import matryoshka.data.Fix
import matryoshka.implicits._
import matryoshka.patterns.EnvT
import scalaz.{Foldable, Monoid}
import scalaz.std.list._
import sql.ast.Ast.Path

sealed trait PlanF[A]

case class Select[A](body: Seq[Projection], child: Option[A]) extends PlanF[A]
case class Where[A](expressions: Seq[Expression], child: Option[A]) extends PlanF[A]
case class From[A](tables: Seq[Path], child: Option[A]) extends PlanF[A]

object Plan {
  type Plan = Fix[PlanF]

  implicit val planFunctor: scalaz.Functor[PlanF] = new scalaz.Functor[PlanF] {
    override def map[A, B](fa: PlanF[A])(f: A => B): PlanF[B] = fa match{
      case Select(body, child) => Select[B](body, child.map(v => f(v)))
      case Where(expressions, child) => Where[B](expressions, child.map(v => f(v)))
      case From(tables, child) => From[B](tables, child.map(v => f(v)))
    }
  }

  implicit val spaceSuffixStringMonoid: scalaz.Monoid[Option[String]] = new Monoid[Option[String]] {
    override def zero: Option[String] = None
    override def append(f1: Option[String], f2: => Option[String]): Option[String] = {
      if(f1.isDefined && f2.isDefined) {
        Some(f1.getOrElse("") + " " + f2.getOrElse(""))
      } else {
        f1.orElse(f2)
      }
    }
  }

//  def labelWithPath[T](implicit T: Recursive.Aux[T, PlanF]): Coalgebra[EnvT[Path, PlanF, ?], (Path, T)] = {
//    case (path, t) =>
//      t.project match {
//        case schema => EnvT((path, schema))
//      }
//  }
  type WithPath[A] = EnvT[String, PlanF, A]

  val planWithPath: Coalgebra[WithPath, (String, Fix[PlanF])] = {
    case (path, Fix(Select(t, child))) => {
      EnvT((
        path,
        Select(t, child.map(f => (s"$path.select", f)))
      ))
    }
    case (path, Fix(Where(t, child))) => {
      EnvT((
        path,
        Where(t, child.map(f => (s"$path.where", f)))
      ))
    }
  }

  def labelWithDepth[T](t: T)(implicit T: Recursive.Aux[T, PlanF]) = {
    T.attributeTopDown[Int](t, 0) {
      case (depth, _) => depth + 1
    }
  }

  def renderSQL: Algebra[PlanF, String] = {
    case Select(body, child) => {
      val children = body.map(_.asSQL).mkString(",")
      val sql =  List(
        Some("SELECT"),
        Some(children),
        child
      )
      Foldable[List].fold(sql)(Monoid[Option[String]]).getOrElse("")
    }
  }

  def select(body: Seq[Projection], child: Option[Plan]): Plan =
    Fix(Select(body, child))

  def where(expressions: Seq[Expression], child: Option[Plan]): Plan =
    Fix(Where(expressions, child))

  def from(tables: Seq[Path], child: Option[Plan]) =
    Fix(From(tables, child))
}