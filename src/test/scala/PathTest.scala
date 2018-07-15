import fastparse.core.Parsed
import pprint.pprintln
import sql.{Ast, Statements}
import utest._

object PathTest extends TestSuite {
  import Ast._

  val tests = Tests {
    'validPath - {
      val Parsed.Success(result, _) = Statements.path.parse("a.b.c.d.e_2")
      assert(result == Path(Seq("a", "b", "c", "d", "e_2")))
    }

    'invalidPath - {
      // This isnt actually valid, deal with it later
      val Parsed.Success(result, _) = Statements.path.parse("a.b.c.d.e-2")
      assert(result == Path(Seq("a", "b", "c", "d", "e")))
    }
  }
}
