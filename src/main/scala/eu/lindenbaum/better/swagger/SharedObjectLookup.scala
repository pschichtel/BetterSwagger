package eu.lindenbaum.better.swagger

case class SharedObjectLookup(schemas: Map[String, ObjectSchema], responses: Map[String, EndpointResponse], parameters: Map[String, EndpointParameter], bodies: Map[String, EndpointBody], headers: Map[String, ResponseHeader]) {
  def withSchema(name: String, objectSchema: ObjectSchema): SharedObjectLookup = copy(schemas = schemas + (name -> objectSchema))
  def withResponse(name: String, response: EndpointResponse): SharedObjectLookup = copy(responses = responses + (name -> response))
  def withParameter(name: String, parameter: EndpointParameter): SharedObjectLookup = copy(parameters = parameters + (name -> parameter))
  def withBody(name: String, body: EndpointBody): SharedObjectLookup = copy(bodies = bodies + (name -> body))
  def withHeader(name: String, header: ResponseHeader): SharedObjectLookup = copy(headers = headers + (name -> header))
}
