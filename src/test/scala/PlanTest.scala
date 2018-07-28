import fastparse.core.Parsed
import utest._
import matryoshka._
import matryoshka.implicits._
import sql.Statements
import sql.ast.Ast.Path
import sql.ast.{Ast, Projection}

object PlanTest extends TestSuite {
  import sql.ast.Plan._

  val tests = Tests {
    'selectTest - {
      val selectPlan = select(
        Seq(
          Projection(Path(Seq("a", "b")), None),
          Projection(Path(Seq("a", "c")), None)
      ), None)

      assert(selectPlan.cata(renderSQL) == "SELECT a.b,a.c")

      val Parsed.Success(result, _) = Statements.selectStatement.parse(selectPlan.cata(renderSQL))
      assert(result == selectPlan)
    }
  }
}
