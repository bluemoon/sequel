import fastparse.core.Parsed
import pprint.pprintln
import sql.Statements
import utest._

object TreeTests extends TestSuite {
  import sql.ast.Ast._

  val tests = Tests {
    'basicTreeFind - {
      val Parsed.Success(result, _) = Statements.selectStatement.parse("select a from table_b where 1=1")
    }
  }
}
