package eu.lindenbaum.better.swagger

sealed trait ObjectSchema {
  def origin: Origin
  def metadata: ObjectMetadata
}
sealed trait Composed { this: ObjectSchema =>
  def terms: Seq[ObjectSchema]
}
final case class MetadataOnlySchema(origin: Origin, `type`: SchemaType[_], metadata: ObjectMetadata) extends ObjectSchema
final case class ReferencedSchema(origin: Origin, metadata: ObjectMetadata, aliasedOrigin: Ref) extends ObjectSchema
final case class PrimitiveSchema(origin: Origin, `type`: SchemaType[_], metadata: ObjectMetadata) extends ObjectSchema
final case class EnumSchema(origin: Origin, `type`: SchemaType[_], metadata: ObjectMetadata, values: Seq[String]) extends ObjectSchema
final case class ConstantSchema(origin: Origin, `type`: SchemaType[_], metadata: ObjectMetadata, value: String) extends ObjectSchema
final case class SingleSchema(origin: Origin, metadata: ObjectMetadata, properties: Map[String, ObjectSchema]) extends ObjectSchema
final case class ProductSchema(origin: Origin, metadata: ObjectMetadata, terms: Seq[ObjectSchema]) extends ObjectSchema with Composed
final case class SumSchema(origin: Origin, metadata: ObjectMetadata, terms: Seq[ObjectSchema]) extends ObjectSchema with Composed
final case class SeqSchema(origin: Origin, metadata: ObjectMetadata, items: ObjectSchema) extends ObjectSchema
