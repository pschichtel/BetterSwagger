package eu.lindenbaum.better.swagger

case class ResponseHeader(name: String, description: Option[String], required: Boolean, deprecated: Boolean,
                          schema: ObjectSchema)
