package sql.ast

import matryoshka._
import matryoshka.data.Fix
import matryoshka.implicits._
import sql.ast.Ast.{Path, Projection, expr}

sealed trait PlanF[A]

case class Select[A](body: Seq[Projection], child: Option[A]) extends PlanF[A]
case class Where[A](expressions: Seq[expr], child: Option[A]) extends PlanF[A]
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

  def renderSQL: Algebra[PlanF, String] = {
    case Select(body, child) => "SELECT"
  }

  def select(body: Seq[Projection], child: Option[Plan]): Plan =
    Fix(Select(body, child))

  def where(expressions: Seq[expr], child: Option[Plan]): Plan =
    Fix(Where(expressions, child))

  def from(tables: Seq[Path], child: Option[Plan]) =
    Fix(From(tables, child))
}