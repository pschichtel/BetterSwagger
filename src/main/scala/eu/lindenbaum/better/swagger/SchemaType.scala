package eu.lindenbaum.better.swagger

sealed trait SchemaType[T]
object SchemaType {
  final case class Str(format: Option[String]) extends SchemaType[String]
  final case class Number(format: Option[String]) extends SchemaType[Either[BigDecimal, BigInt]]
  final case class Integer(format: Option[String]) extends SchemaType[BigInt]
  final case class Bool(format: Option[String]) extends SchemaType[BigInt]
  final case class Array[C <: SchemaType[_]](itemType: C) extends SchemaType[Seq[C]]
  case object Object extends SchemaType[Any]
  final case class Unspecified(format: Option[String]) extends SchemaType[Nothing]
  final case class Unknown(name: String, format: Option[String]) extends SchemaType[Any]
}
