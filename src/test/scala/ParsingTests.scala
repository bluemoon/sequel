import fastparse.core.Parsed
import pprint.pprintln
import sql.Statements
import sql.ast.Ast
import sql.ast.Ast.{Path, Projection, identifier}
import utest._

object ParsingTests extends TestSuite {
  import sql.ast.Ast
  import sql.ast.Plan._

  val tests = Tests {
    'basicProjection - {
      val Parsed.Success(result, _) = Statements.projection.parse("bob as bobby")
      assert(result == Ast.Projection(Ast.Path(Seq("bob")), Some(Ast.identifier("bobby"))))
    }

    'basicSelectCaseInsensitive - {
      val Parsed.Success(result, _) = Statements.selectStatement.parse("SELECT a FROM bob")
      assert(result == select(
        Seq(Ast.Projection(Ast.Path(Seq("a")), None)),
        Some(from(Seq(Ast.Path(Seq("bob"))), None)))
      )
    }

    'basicSelect - {
      val Parsed.Success(result, _) = Statements.selectStatement.parse("select a from bob")
      assert(result == select(
          Seq(Ast.Projection(Ast.Path(Seq("a")), None)),
          Some(from(Seq(Ast.Path(Seq("bob"))), None))
        )
      )
    }

    'selectWithProjectedPath - {
      val Parsed.Success(result, _) = Statements.selectStatement.parse("select a.b.c as f from bob")
      assert(result == select(
        Seq(Projection(Path(Seq("a", "b", "c")), Some(identifier("f")))),
        Some(from(Seq(Path(Seq("bob"))), None))
      ))
    }

    'manyProjectedSelect - {
      val Parsed.Success(result, _) = Statements.selectStatement.parse("select a, b.m, c, d, e as t from bob")
      assert(result == select(
        Seq(
          Ast.Projection(Ast.Path(Seq("a")), None),
          Ast.Projection(Ast.Path(Seq("b", "m")), None),
          Ast.Projection(Ast.Path(Seq("c")), None),
          Ast.Projection(Ast.Path(Seq("d")), None),
          Ast.Projection(Ast.Path(Seq("e")), Some(Ast.identifier("t")))
        ),
        Some(from(Seq(Ast.Path(Seq("bob"))), None))
      ))
    }

    'selectWithWhere - {
      val Parsed.Success(result, _) = Statements.selectStatement.parse("select a from table_b where 1=1")
      assert(result == select(
        Seq(Ast.Projection(Ast.Path(Seq("a")), None)),
        Some(
          from(
            Seq(Ast.Path(Seq("table_b"))),
            Some(where(Seq(Ast.expression.Compare("1", List(Ast.ComparisonOp.Eq), List("1"))), None))
          )
        )
      )
      )
    }
  }
}