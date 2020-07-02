package eu.lindenbaum.better.swagger

sealed trait ObjectSchema {
  def origin: Origin
}
sealed trait Composed { this: ObjectSchema =>
  def terms: Seq[ObjectSchema]
}
final case class SchemaReference(origin: Origin, aliasedOrigin: Ref) extends ObjectSchema
final case class PrimitiveSchema(origin: Origin, `type`: String, format: Option[String]) extends ObjectSchema
final case class EnumSchema(origin: Origin, `type`: Option[String], format: Option[String], values: Seq[String]) extends ObjectSchema
final case class ConstantSchema(origin: Origin, `type`: Option[String], format: Option[String], value: String) extends ObjectSchema
final case class SingleSchema(origin: Origin, properties: Map[String, ObjectSchema]) extends ObjectSchema
final case class ProductSchema(origin: Origin, terms: Seq[ObjectSchema]) extends ObjectSchema with Composed
final case class SumSchema(origin: Origin, terms: Seq[ObjectSchema]) extends ObjectSchema with Composed

sealed trait Origin {
  def / (scopeName: String): Origin = Scope(scopeName, this)

  def source: Option[SourceFile] = this match {
    case Root => None
    case s: SourceFile => Some(s)
    case Scope(_, parent) => parent.source
  }
}

case object Root extends Origin
final case class SourceFile(file: Source, origin: Origin) extends Origin {
  override def toString: String = s"$origin -> <${file.value}>"
}
final case class Scope(name: String, origin: Origin) extends Origin {
  override def toString: String = s"""$origin -> "$name""""
}
