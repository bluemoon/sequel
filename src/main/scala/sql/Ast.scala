package sql

import fastparse.all._

object Ast {
  case class identifier(name: String)

  sealed trait cmpop

  object cmpop {
    case object Eq extends cmpop
    case object NotEq extends cmpop
    case object Lt extends cmpop
    case object LtE extends cmpop
    case object Gt extends cmpop
    case object GtE extends cmpop
    case object Is extends cmpop
    case object IsNot extends cmpop
    case object In extends cmpop
    case object NotIn extends cmpop
  }

  sealed trait boolop

  object boolop {
    case object And extends boolop
    case object Or extends boolop
  }

  sealed trait expr
  object expression {
    case class Num(n: Any) extends expr
    case class Str(s: String) extends expr
    case class BoolOp(op: boolop, values: Seq[Any]) extends expr
    case class BinaryOp(left: expr, right: expr) extends expr
    case class Compare(left: Any, ops: Seq[cmpop], comparators: Seq[Any]) extends expr
  }

  case class Path(seq: Seq[String])

  sealed trait Plan

  case class Select(body: Seq[Projection], child: Option[Plan]) extends Plan {
    override def toString: String = {
      body.foldLeft("SELECT ")((b, a) => b + a)
    }
  }

  case class Where(expressions: Seq[expr]) extends Plan
  case class From(tables: Seq[Path], child: Option[Plan]) extends Plan

  case class Projection(path: Path, as: Option[identifier]) {
    override def toString: String = {
      val asValue = as.fold("")(f => s" as ${f.name}")
      s"$path$asValue"
    }
  }
}
