package eu.lindenbaum.better.swagger

case class EndpointParameter(name: String, kind: ParameterKind, description: Option[String], required: Boolean,
                             deprecated: Boolean, explode: Boolean, allowReserved: Boolean, allowEmptyValue: Boolean,
                             schema: ObjectSchema)
