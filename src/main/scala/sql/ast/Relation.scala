package sql.ast


sealed abstract class JoinType extends Product with Serializable

object JoinType {
  final case object Inner extends JoinType
  final case object FullOuter extends JoinType
  final case object LeftOuter extends JoinType
  final case object RightOuter extends JoinType
}

sealed trait Relation[A]

case class IdentRelation[A](projection: Projection) extends Relation[A]
case class JoinRelation[A](tables: A, tpe: JoinType, how: Seq[Expression]) extends Relation[A]
case class ExpressionRelation[A](expressions: Seq[Expression])
case class RelationGroup[A](relations: Seq[A]) extends Relation[A]

object Relation {

}
