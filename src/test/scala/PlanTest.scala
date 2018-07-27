import fastparse.core.Parsed
import pprint.pprintln
import sql.Statements
import sql.ast.Ast
import utest._

object PlanTest extends TestSuite {
  import sql.ast.Plan._

  val tests = Tests {
    'exprTest - {
      select(Seq.empty[Ast.Projection], None)
    }
  }
}
