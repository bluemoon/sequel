package sql

import fastparse.noApi._
import WsApi._
import sql.ast.{Ast, Plan, Projection, Expression}

object Statements {
  val path = P(Lexical.identifier.rep(min = 1, sep=".")).map {
    case (p) => Ast.Path(p.map(_.name))
  }

  val aliasStatement = P(
    Lexical.kw("as") ~
    Lexical.identifier
  )

  val projection = P(path ~ aliasStatement.?).map {
    case (p, as) => Projection(p, as)
  }

  val tableList = P(path ~ Lexical.space.?)

  val comparable = P(Lexical.identifier | Lexical.float | Lexical.digit.rep)
  val comparison = P(comparable.! ~ Lexical.comparison_operators ~ comparable.!).map {
    case (lhs, op, rhs) => Expression.Compare(lhs, Seq(op), Seq(rhs))
  }

  val notStatement: Parser[Expression] = P((Lexical.kw("not") ~ notStatement) | comparison)
  val andStatement = P(notStatement.rep(1, Lexical.kw("and"))).map {
    case Seq(x) => x
    case xs => Expression.BoolOp(Ast.boolop.And, xs)
  }
  val orStatement = P(andStatement.rep(1, Lexical.kw("or"))).map {
    case Seq(x) => x
    case xs => Expression.BoolOp(Ast.boolop.Or, xs)
  }

  val expression = orStatement

  val whereStatement = P(Lexical.kw("where") ~ expression.rep(min = 1)).map {
    case (comparisons) => Plan.where(comparisons, None)
  }

  val fromStatement = P(Lexical.kw("from") ~ tableList.rep(sep = ",") ~ whereStatement.?).map {
    case (tables, where) => Plan.from(tables, where)
  }

  val selectStatement = P(
    Lexical.kw("select") ~ projection.rep(sep = ",") ~ fromStatement.?).map {
    case (projections, from) => Plan.select(projections, from)
  }
}

