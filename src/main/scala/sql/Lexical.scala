package sql

object WsApi extends fastparse.WhitespaceApi.Wrapper(Lexical.ws)

object Lexical {
  import fastparse.all._

  val space = P(CharIn(" \n"))
  val ws = P((CharsWhileIn(" \n") |  "\\\n").rep)
  val comment = P("--" ~ CharsWhile(_ != '\n', min = 0))

  def kw(s: String) = IgnoreCase(s) ~ !(letter | digit | "_")

  val keywords = Set(
    "all", "and", "as", "asc", "between", "by", "case", "cast", "cross", "cube",
    "desc", "distinct", "else", "end", "exists", "false", "from", "full", "group", "grouping",
    "having", "in", "inner", "is", "join", "left", "like", "limit",
    "not", "null", "on", "or", "order", "outer", "right", "rollup", "select",
    "sets", "then", "true", "union", "unknown", "when", "where"
  )

  val lowercase = P(CharIn('a' to 'z'))
  val uppercase = P(CharIn('A' to 'Z'))
  val digit = P(CharIn('0' to '9'))
  val letter = P(lowercase | uppercase)

  val identifier: P[Ast.identifier] =
    P((letter | "_") ~ (letter | digit | "_").rep).!.filter(!keywords.contains(_)).map(Ast.identifier)

  def op[T](s: P0, rhs: T) = s.!.map(_ => rhs)
  val Eq = op("=", Ast.cmpop.Eq)

  val comparison_operators = P(Eq)

//  val Lt = op("<", Ast.cmpop.Lt)
//  val Gt = op(">", Ast.cmpop.Gt)
//  val GtE = op(">=", Ast.cmpop.GtE)
//  val LtE = op("<=", Ast.cmpop.LtE)
//  val NotEq = op("<>" | "!=", Ast.cmpop.NotEq)
//  val In = op("in", Ast.cmpop.In)
//  val NotIn = op("not" ~ "in", Ast.cmpop.NotIn)
//  val Is = op("is", Ast.cmpop.Is)
//  val IsNot = op("is" ~ "not", Ast.cmpop.IsNot)
//  val comp_op = P(LtE | GtE | Eq | Gt | Lt | NotEq | In | NotIn | IsNot | Is)

  val intpart = P(digit.rep(1))
  val fraction = P("." ~ digit.rep(1))
  val float = P(intpart.? ~ fraction | intpart ~ ".")
}
