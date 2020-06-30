package eu.lindenbaum.better.swagger

sealed trait ObjectSchema {
  def origin: Origin
}
sealed trait Composed { this: ObjectSchema =>
  def terms: Seq[ObjectSchema]
}
final case class SimpleSchema(origin: Origin) extends ObjectSchema
final case class ProductSchema(origin: Origin, terms: Seq[ObjectSchema]) extends ObjectSchema with Composed
final case class SumSchema(origin: Origin, terms: Seq[ObjectSchema]) extends ObjectSchema with Composed

sealed trait Origin {
  def / (scopeName: String): Origin = Scope(scopeName, this)
}
object Origin {
  def fromRef(r: String): Option[Origin] = {
    val (_, ref) = splitReference(r)

    if (ref.isEmpty) None
    else {
      val lastSlash = ref.lastIndexOf('/')
      if (lastSlash == -1) None
      else {
        Some(Root / ref.substring(lastSlash + 1))
      }
    }
  }
}
case object Root extends Origin
final case class SourceFile(ref: String, origin: Origin) extends Origin {
  override def toString: String = s"$origin -> <$ref>"
}
final case class Scope(name: String, origin: Origin) extends Origin {
  override def toString: String = s"$origin -> $name"
}
