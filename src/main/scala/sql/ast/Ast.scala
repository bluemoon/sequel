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


  case class Path(seq: Seq[String])
}
