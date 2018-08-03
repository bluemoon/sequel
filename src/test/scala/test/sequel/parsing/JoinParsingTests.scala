package test.sequel.parsing

import pprint.pprintln
import utest._

import sql.Statements


object JoinParsingTests extends TestSuite {
  import sql.ast.Ast
  import sql.ast.Plan._
  import sql.ast.Projection

  val tests = Tests {
    'innerJoinSelect - {
      val query =
        """
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
      pprintln(result, height=200)
      assert(false)
    }
  }
}
