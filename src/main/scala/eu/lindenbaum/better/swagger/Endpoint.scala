package eu.lindenbaum.better.swagger

case class Endpoint(id: String, method: String, path: String, description: Option[String], tags: Set[String],
                    params: Seq[EndpointParameter], responses: Map[Int, EndpointResponse])
