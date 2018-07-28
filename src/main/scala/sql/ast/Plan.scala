package sql.ast

import matryoshka._
import matryoshka.data.Fix
import matryoshka.implicits._
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