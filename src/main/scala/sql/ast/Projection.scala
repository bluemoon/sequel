package sql.ast

import scalaz.Monoid
import scalaz._
import scalaz.Scalaz._
import sql.ast.Ast.{Path, identifier}


case class Projection(path: Path, as: Option[identifier]) {
  def asSQL: String = {
    val p = path.seq.mkString(".")
    Monoid[Option[String]].append(Some(p), as.map(f => s"$p as $f")).getOrElse("")
  }
}


