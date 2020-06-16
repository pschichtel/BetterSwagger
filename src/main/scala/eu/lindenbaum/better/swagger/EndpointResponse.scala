package eu.lindenbaum.better.swagger

case class EndpointResponse(statusCode: Int, description: Option[String], headers: Map[String, ResponseHeader])
