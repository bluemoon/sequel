package sql.ast

import sql.ast.Ast.{ComparisonOp, boolop}


sealed trait Expression

object Expression {
  case class Num(n: Any) extends Expression
  case class Str(s: String) extends Expression
  case class BoolOp(op: boolop, values: Seq[Any]) extends Expression
  case class BinaryOp(left: Expression, right: Expression) extends Expression
  case class Compare(left: Any, ops: Seq[ComparisonOp], comparators: Seq[Any]) extends Expression
}
