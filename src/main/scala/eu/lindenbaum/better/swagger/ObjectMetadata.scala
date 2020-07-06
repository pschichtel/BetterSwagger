package eu.lindenbaum.better.swagger

case class ObjectMetadata(name: Option[String], title: Option[String], description: Option[String], deprecated: Boolean,
                          default: Option[Any], requiredProperties: Seq[String])