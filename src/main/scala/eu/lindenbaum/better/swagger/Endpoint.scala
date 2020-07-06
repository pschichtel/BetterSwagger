package eu.lindenbaum.better.swagger

case class Endpoint(id: String, method: String, path: String, summary: Option[String], description: Option[String],
                    tags: Set[String], params: Seq[EndpointParameter], requestBodies: Map[String, EndpointBody],
                    responseBodies: Map[Int, Map[String, EndpointResponse]])
