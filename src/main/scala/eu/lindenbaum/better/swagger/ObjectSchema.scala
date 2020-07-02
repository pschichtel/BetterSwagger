package eu.lindenbaum.better.swagger

sealed trait ObjectSchema {
  def origin: Origin
  def metadata: ObjectMetadata
}
sealed trait Composed { this: ObjectSchema =>
  def terms: Seq[ObjectSchema]
}
final case class MetadataOnlySchema(origin: Origin, metadata: ObjectMetadata) extends ObjectSchema
final case class SchemaReferenceSchema(origin: Origin, metadata: ObjectMetadata, aliasedOrigin: Ref) extends ObjectSchema
final case class PrimitiveSchema(origin: Origin, metadata: ObjectMetadata, `type`: String, format: Option[String]) extends ObjectSchema
final case class EnumSchema(origin: Origin, metadata: ObjectMetadata, `type`: Option[String], format: Option[String], values: Seq[String]) extends ObjectSchema
final case class ConstantSchema(origin: Origin, metadata: ObjectMetadata, `type`: Option[String], format: Option[String], value: String) extends ObjectSchema
final case class SingleSchema(origin: Origin, metadata: ObjectMetadata, properties: Map[String, ObjectSchema]) extends ObjectSchema
final case class ProductSchema(origin: Origin, metadata: ObjectMetadata, terms: Seq[ObjectSchema]) extends ObjectSchema with Composed
final case class SumSchema(origin: Origin, metadata: ObjectMetadata, terms: Seq[ObjectSchema]) extends ObjectSchema with Composed

case class ObjectMetadata(name: Option[String], description: Option[String])

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
