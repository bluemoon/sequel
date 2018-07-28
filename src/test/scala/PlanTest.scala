import utest._
import matryoshka._
import matryoshka.implicits._
import sql.ast.Ast

object PlanTest extends TestSuite {
  import sql.ast.Plan._

  val tests = Tests {
    'exprTest - {
      val selectPlan = select(Seq.empty[Ast.Projection], None)
      assert(selectPlan.cata(renderSQL) == "SELECT")
    }
  }
}
