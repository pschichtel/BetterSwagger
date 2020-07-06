package eu.lindenbaum.better.swagger

case class EndpointResponse(statusCode: Int, mediaType: String, description: Option[String],
                            headers: Map[String, ResponseHeader], schema: ObjectSchema)
