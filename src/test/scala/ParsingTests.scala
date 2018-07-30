import fastparse.core.Parsed
import pprint.pprintln
import sql.Statements
import sql.ast.{Ast, Expression}
import sql.ast.Ast.{Path, identifier}
import utest._

object ParsingTests extends TestSuite {
  import sql.ast.Ast
  import sql.ast.Plan._
  import sql.ast.Projection

  val tests = Tests {
    'basicProjection - {
      val Parsed.Success(result, _) = Statements.projection.parse("bob as bobby")
      assert(result == Projection(Ast.Path(Seq("bob")), Some(Ast.identifier("bobby"))))
    }

    'basicSelectCaseInsensitive - {
      val Parsed.Success(result, _) = Statements.selectStatement.parse("SELECT a FROM bob")
      assert(result == select(
        Seq(Projection(Ast.Path(Seq("a")), None)),
        Some(from(Seq(Ast.Path(Seq("bob"))), None)))
      )
    }

    'basicSelect - {
      val Parsed.Success(result, _) = Statements.selectStatement.parse("select a from bob")
      assert(result == select(
          Seq(Projection(Ast.Path(Seq("a")), None)),
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
          Projection(Ast.Path(Seq("a")), None),
          Projection(Ast.Path(Seq("b", "m")), None),
          Projection(Ast.Path(Seq("c")), None),
          Projection(Ast.Path(Seq("d")), None),
          Projection(Ast.Path(Seq("e")), Some(Ast.identifier("t")))
        ),
        Some(from(Seq(Ast.Path(Seq("bob"))), None))
      ))
    }

    'selectWithWhere - {
      val Parsed.Success(result, _) = Statements.selectStatement.parse("select a from table_b where 1=1")
      assert(result == select(
        Seq(Projection(Ast.Path(Seq("a")), None)),
        Some(
          from(
            Seq(Ast.Path(Seq("table_b"))),
            Some(where(Seq(Expression.Compare("1", List(Ast.ComparisonOp.Eq), List("1"))), None))
          )
        )
      )
      )
    }

    'complexSelect - {
      val query = """
      SELECT DISTINCT
      d.id AS id,
      d.account_name AS account_name,
      d.table_name AS table_name,
      d.asset_class AS asset_class,
      d.data_type AS data_type,
      d.platform AS platform,
      odg.is_owner AS is_owner,
      d.can_be_requested AS can_be_requested,
      d.waterfall_preview_available AS waterfall_preview_available
      FROM organization_datasets AS odg
        INNER JOIN dataset_group_datasets AS dgd
      ON odg.dataset_group_id = dgd.dataset_group_id
      INNER JOIN dataset_groups AS dg
      ON dg.id = odg.dataset_group_id
      INNER JOIN datasets AS d
      ON dgd.dataset_id = d.id
      WHERE account_name in ('ASDF 2042-BS7');
      """

      val result = Statements.selectStatement.parse(query)
      pprintln(result)
      assert(false)
    }
  }
}