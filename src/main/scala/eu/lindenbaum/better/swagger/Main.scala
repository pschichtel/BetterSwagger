package eu.lindenbaum.better.swagger

import io.swagger.v3.oas.models.headers.Header
import io.swagger.v3.oas.models.parameters.{Parameter, RequestBody}
import io.swagger.v3.oas.models.responses.ApiResponse
import io.swagger.v3.oas.models.{OpenAPI, Operation, PathItem, Paths}
import io.swagger.v3.parser.OpenAPIV3Parser
import java.util.{Map => JMap}

import scala.jdk.CollectionConverters._



object Main {

  def parseEndpoints(paths: Paths): Result[Seq[Endpoint]] = {
    val endpoints = Option(paths)
      .map(_.asScala.toSeq)
      .getOrElse(Seq.empty)
      .map((parsePath _).tupled)

    Result.sequence(endpoints).map(_.flatten)
  }

  def parsePath(path: String, item: PathItem): Result[Seq[Endpoint]] = {

    def op(method: String, op: Operation): Option[(String, Operation)] =
      Option(op).map(o => (method, o))

    val ops = Seq(
      op("GET", item.getGet),
      op("POST", item.getPost),
      op("PATCH", item.getPatch),
      op("DELETE", item.getDelete),
      op("OPTIONS", item.getOptions),
      op("HEAD", item.getHead),
      op("PUT", item.getPut),
      op("TRACE", item.getTrace)
    ).flatten

    Result.sequence(for ((method, op) <- ops) yield parseOperation(method, path, op))
  }

  def parseOperation(method: String, path: String, op: Operation): Result[Endpoint] = {
    val desc = Option(op.getDescription)
    val summary = Option(op.getSummary)
    val id = op.getOperationId
    if (id == null) {
      Result.error("No operationId given!")
    } else {

      val tags = Option(op.getTags).map(_.asScala.toSet).getOrElse(Set.empty)

      for {
        params <- parseParameters(op)
        responses <- parseResponses(op)
      } yield Endpoint(id, method, path, summary, desc, tags, params, responses)
    }
  }

  def parseParameters(op: Operation): Result[Seq[EndpointParameter]] = {
    val params = Option(op.getParameters)
      .map(_.asScala.toSeq)
      .getOrElse(Nil)
      .map(p => parseParameter(p))
    Result.sequence(params)
  }

  def parseParameter(p: Parameter): Result[EndpointParameter] = {
    val description = Option(p.getDescription)
    val required = p.getRequired
    val deprecated = p.getDeprecated
    val explode = p.getExplode
    val allowReserved = p.getAllowReserved
    val allowEmptyValue = p.getAllowEmptyValue

    for {
      name <- Result.required(p.getName, "name is required")
      kind <- ParameterKind(p.getIn)
    } yield EndpointParameter(name, kind, description, required, deprecated, explode, allowReserved, allowEmptyValue)
  }

  def parseResponses(op: Operation): Result[Map[Int, EndpointResponse]] = {
    val responses = Option(op.getResponses)
      .map(_.asScala.toSeq)
      .getOrElse(Seq.empty)
      .map((parseResponse _).tupled)

    Result.sequence(responses).map(_.toMap)
  }

  def parseResponse(name: String, response: ApiResponse): Result[(Int, EndpointResponse)] = {
    parseInt(name) map { statusCode =>
      val description = Option(response.getDescription)
      val headers = parseHeaders(response.getHeaders)

      (statusCode, EndpointResponse(statusCode, description, headers))
    }
  }

  def parseHeaders(headers: JMap[String, Header]): Map[String, ResponseHeader] = {
    Option(headers)
      .map(_.asScala.toSeq)
      .getOrElse(Seq.empty)
      .map((parseHeader _).tupled)
      .toMap
  }

  def parseHeader(name: String, header: Header): (String, ResponseHeader) = {
    val desc = Option(header.getDescription)
    val required = header.getRequired
    val deprecated = header.getDeprecated

    (name, ResponseHeader(name, desc, required, deprecated))
  }

  def parseInt(s: String): Result[Int] = {
    try {
      Result.ok(Integer.parseInt(s))
    } catch {
      case e: NumberFormatException => Result.error(e.getLocalizedMessage)
    }
  }

  def parseRequestBodies(openAPI: OpenAPI): Result[Map[String, EndpointBody]] = {
    val bodies = Option(openAPI.getComponents)
      .flatMap(c => Option(c.getRequestBodies))
      .map(_.asScala.toSeq)
      .getOrElse(Seq.empty)
      .map { case (name, body) => parseRequestBody(body).map(r => (name, r)) }

    Result.sequence(bodies).map(_.toMap)
  }

  def parseRequestBody(body: RequestBody): Result[EndpointBody] = {
    ???
  }

  def main(args: Array[String]): Unit = {
    val openAPI = new OpenAPIV3Parser().read("https://stage.cognitivevoice.io/v1/docs/specs/core.yaml")

    parseEndpoints(openAPI.getPaths) match {
      case Right(endpoints) => endpoints.foreach(println)
      case Left(errors) => println(errors)
    }

    val headers = parseHeaders(openAPI.getComponents.getHeaders)
    println(s"headers: $headers")

    val requestBodies = openAPI.getComponents.getRequestBodies
    println(requestBodies)


    openAPI.getComponents.getSchemas.asScala.foreach { case (order, schema) =>
      println(s"$order - ${schema.get$ref()}")
    }
  }
}
