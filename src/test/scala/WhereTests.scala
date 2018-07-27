import fastparse.core.Parsed
import pprint.pprintln
import sql.Statements
import utest._

object WhereTests extends TestSuite {
  import sql.ast.Ast._
  val tests = Tests {
    'simpleWhereEquality - {
      val Parsed.Success(result, _) = Statements.whereStatement.parse("where bob = 1")
      assert(result == Where(Seq(expression.Compare("bob", List(ComparisonOp.Eq), List("1"))), None))
    }
    'andWhereEquality - {
      val Parsed.Success(result, _) = Statements.whereStatement.parse("where bob = 1 and thing = 1")
      assert(result ==
        Where(Seq(expression.BoolOp(boolop.And, Seq(
          expression.Compare("bob", List(ComparisonOp.Eq), List("1")),
          expression.Compare("thing", List(ComparisonOp.Eq), List("1"))))), None))
    }
  }
}
