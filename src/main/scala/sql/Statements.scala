package sql

import fastparse.noApi._
import WsApi._
import fastparse.{core, noApi}
import matryoshka.data.Fix
import sql.ast.Plan.Plan
import sql.ast.{Ast, Expression, IdentRelation, JoinRelation, JoinType, Plan, PlanF, Projection, Relation, RelationGroup}

object Statements {
  val path: core.Parser[Ast.Path, Char, String] = P(Lexical.identifier.rep(min = 1, sep=".").log()).map {
    case (p) => Ast.Path(p.map(_.name))
  }

  val aliasStatement: noApi.Parser[Ast.identifier] = P(
    Lexical.kw("as") ~
    Lexical.identifier
  )

  val projection: core.Parser[Projection, Char, String] = P(path ~ aliasStatement.?).log().map {
    case (p, as) => {
      println(p)
      Projection(p, as)
    }
  }

  val tableList = P(projection.rep(sep = ",")).map {
    case tables => tables.map(IdentRelation(_))
  }

  val list = P( "(" ~ (Lexical.singleQuotedString | Lexical.identifier).rep(sep = ",") ~ ")" ).log()
  val comparable: noApi.Parser[Any] = P(path | Lexical.identifier | Lexical.float | Lexical.digit.rep | list).log()
  val comparison: core.Parser[Expression.Compare, Char, String] = P(comparable.! ~ Lexical.comparison_operators ~ comparable.!).map {
    case (lhs, op, rhs) => Expression.Compare(lhs, Seq(op), Seq(rhs))
  }

  val notStatement: Parser[Expression] = P((Lexical.kw("not") ~ notStatement) | comparison)
  val andStatement: core.Parser[Expression, Char, String] = P(notStatement.rep(1, Lexical.kw("and"))).map {
    case Seq(x) => x
    case xs => Expression.BoolOp(Ast.boolop.And, xs)
  }

  val orStatement: core.Parser[Expression, Char, String] = P(andStatement.rep(1, Lexical.kw("or"))).map {
    case Seq(x) => x
    case xs => Expression.BoolOp(Ast.boolop.Or, xs)
  }

  val expression: core.Parser[Expression, Char, String] = orStatement

  val whereStatement: core.Parser[Plan, Char, String] = P(Lexical.kw("where") ~ expression.rep(min = 1)).map {
    case (comparisons) => Plan.where(comparisons, None)
  }

  /**
    * left LEFT JOIN right
    * left RIGHT JOIN right
    * left OUTER JOIN right
    * left INNER JOIN right
    * left INNER JOIN right ON left.thing = right.thing
    * left INNER JOIN right ON left.thing = right.thing INNER JOIN
    * left FULL OUTER JOIN right
    *
    * Not captured here but still a join:
    * from left, right where left.a = right.b
    *
    * the left is EITHER a previous join or a projection
  */
  val join = P(
      Lexical.kw("INNER JOIN") |
      Lexical.kw("FULL OUTER JOIN") |
      Lexical.kw("OUTER JOIN") |
      Lexical.kw("LEFT JOIN") |
      Lexical.kw("RIGHT JOIN")
  )

  /**
    * (projection.?) (left|right|inner) join (projection) on (expression)
    */
  val joinStatement: noApi.Parser[Relation[_]] = P(
      projection.? ~
      join ~
      projection ~
      Lexical.kw("on") ~
      expression ~
      joinStatement.?
  ).map {

    case (None, projection: Projection, expressions, subRelation) => {
      val id = IdentRelation(projection)
      JoinRelation(Seq(id, subRelation), JoinType.Inner, Seq(expressions))
    }

    case (Some(proj), rightProjection: Projection, expressions, subRelation) => {
      val leftRelation = IdentRelation(proj)
      val rightRelation = IdentRelation(rightProjection)

      val joinList = subRelation match {
        case Some(sub) => Seq(rightRelation, sub)
        case None => Seq(rightRelation)
      }

      val subJoin = JoinRelation(
        joinList,
        JoinType.Inner,
        Seq(expressions)
      )

      JoinRelation(Seq(leftRelation, subJoin), JoinType.Inner, Seq())

    }
  }

  val tablesStatement: noApi.Parser[Relation[_]] = P(joinStatement.rep | tableList).map {
    case joins: Seq[Relation[_]] => RelationGroup(joins)
    case tableList: IdentRelation[_] => tableList
  }

  val fromStatement: core.Parser[Fix[PlanF], Char, String] = P(Lexical.kw("from") ~ tablesStatement ~ whereStatement.?).map {
    case (tables, where) => Plan.from(tables, where)
  }

  val selectStatement = P(Lexical.ws ~
    Lexical.kw("select") ~ Lexical.kw("distinct").? ~ projection.rep(sep = ",") ~ fromStatement.?).map {
    case (projections, from) => Plan.select(projections, from)
  }
}

