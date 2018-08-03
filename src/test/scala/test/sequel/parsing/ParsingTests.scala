package test.sequel.parsing

import fastparse.core.Parsed
import sql.Statements
import sql.ast.Ast.{Path, identifier}
import sql.ast.JoinType.Inner
import sql.ast.{Expression, IdentRelation, JoinRelation}
import utest._

object ParsingTests extends TestSuite {
  import sql.ast.Plan._
  import sql.ast.{Ast, Projection}

  val tests = Tests {
    'basicProjection - {
      val Parsed.Success(result, _) = Statements.projection.parse("bob as bobby")
      assert(result == Projection(Ast.Path(Seq("bob")), Some(Ast.identifier("bobby"))))
    }

    'basicSelectCaseInsensitive - {
      val Parsed.Success(result, _) = Statements.selectStatement.parse("SELECT a FROM bob")
      assert(result == select(Seq(Projection(Path(Seq("a")),None)),
        Some(from(JoinRelation(IdentRelation(Projection(Path(Seq("bob")),None)), Inner, Seq()),None
      )))
      )
    }

    'basicSelect - {
      val Parsed.Success(result, _) = Statements.selectStatement.parse("select a from bob")
      assert(result == select(
          Seq(Projection(Ast.Path(Seq("a")), None)),
          Some(from(IdentRelation(Projection(Path(Seq("bob")), None)), None))
        )
      )
    }

    'selectWithProjectedPath - {
      val Parsed.Success(result, _) = Statements.selectStatement.parse("select a.b.c as f from bob")
      assert(result == select(
        Seq(Projection(Path(Seq("a", "b", "c")), Some(identifier("f")))),
        Some(from(IdentRelation(Projection(Ast.Path(Seq("bob")), None)), None))
      ))
    }

    'manyProjectedSelect - {
      val Parsed.Success(result, _) = Statements.selectStatement.parse("select a, b.m, c, d, e as t from bob")
      assert(result == select(
        Seq(
          Projection(Ast.Path(Seq("a")), None),
          Projection(Ast.Path(Seq("b", "m")), None),
          Projection(Ast.Path(Seq("c")), None),
          Projection(Ast.Path(Seq("d")), None),
          Projection(Ast.Path(Seq("e")), Some(Ast.identifier("t")))
        ),
        Some(from(IdentRelation(Projection(Ast.Path(Seq("bob")), None)), None))
      ))
    }

    'selectWithWhere - {
      val Parsed.Success(result, _) = Statements.selectStatement.parse("select a from table_b where 1=1")
      assert(result == select(
        Seq(Projection(Ast.Path(Seq("a")), None)),
        Some(
          from(
            IdentRelation(Projection(Ast.Path(Seq("table_b")), None)),
            Some(where(Seq(Expression.Compare("1", List(Ast.ComparisonOp.Eq), List("1"))), None))
          )
        )
      )
      )
    }
  }
}
