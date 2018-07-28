import fastparse.core.Parsed
import sql.Statements
import sql.ast.Expression
import utest._

object WhereTests extends TestSuite {
  import sql.ast.Ast._
  import sql.ast.Plan.where

  val tests = Tests {
    'simpleWhereEquality - {
      val Parsed.Success(result, _) = Statements.whereStatement.parse("where bob = 1")
      assert(result == where(Seq(Expression.Compare("bob", List(ComparisonOp.Eq), List("1"))), None))
    }
    'andWhereEquality - {
      val Parsed.Success(result, _) = Statements.whereStatement.parse("where bob = 1 and thing = 1")
      assert(result ==
        where(Seq(Expression.BoolOp(boolop.And, Seq(
          Expression.Compare("bob", List(ComparisonOp.Eq), List("1")),
          Expression.Compare("thing", List(ComparisonOp.Eq), List("1"))))), None))
    }
  }
}
