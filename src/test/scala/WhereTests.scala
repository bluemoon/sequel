import fastparse.core.Parsed
import pprint.pprintln
import sql.Statements
import utest._

object WhereTests extends TestSuite {
  import sql.ast.Ast._
  import sql.ast.Plan.where

  val tests = Tests {
    'simpleWhereEquality - {
      val Parsed.Success(result, _) = Statements.whereStatement.parse("where bob = 1")
      assert(result == where(Seq(expression.Compare("bob", List(ComparisonOp.Eq), List("1"))), None))
    }
    'andWhereEquality - {
      val Parsed.Success(result, _) = Statements.whereStatement.parse("where bob = 1 and thing = 1")
      assert(result ==
        where(Seq(expression.BoolOp(boolop.And, Seq(
          expression.Compare("bob", List(ComparisonOp.Eq), List("1")),
          expression.Compare("thing", List(ComparisonOp.Eq), List("1"))))), None))
    }
  }
}
