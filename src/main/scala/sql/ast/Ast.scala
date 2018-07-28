package sql.ast


object Ast {
  case class identifier(name: String)

  sealed trait AstNode

  sealed trait ComparisonOp extends AstNode

  object ComparisonOp {
    case object Eq extends ComparisonOp
    case object NotEq extends ComparisonOp
    case object Lt extends ComparisonOp
    case object LtE extends ComparisonOp
    case object Gt extends ComparisonOp
    case object GtE extends ComparisonOp
    case object Is extends ComparisonOp
    case object IsNot extends ComparisonOp
    case object In extends ComparisonOp
    case object NotIn extends ComparisonOp
  }

  sealed trait boolop

  object boolop {
    case object And extends boolop
    case object Or extends boolop
  }

  sealed trait expr // extends TreeNode[expr]
  object expression {
    case class Num(n: Any) extends expr
    case class Str(s: String) extends expr
    case class BoolOp(op: boolop, values: Seq[Any]) extends expr
    case class BinaryOp(left: expr, right: expr) extends expr
    case class Compare(left: Any, ops: Seq[ComparisonOp], comparators: Seq[Any]) extends expr
  }

  case class Path(seq: Seq[String])
}
