package eu.lindenbaum.better.swagger

import io.swagger.v3.oas.models.headers.Header
import io.swagger.v3.oas.models.parameters.{Parameter, RequestBody}
import io.swagger.v3.oas.models.responses.ApiResponse
import io.swagger.v3.oas.models.{OpenAPI, Operation, PathItem, Paths}
import java.util.{Map => JMap}

import io.swagger.v3.oas.models.media.{ComposedSchema, Schema}

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
      Error("No operationId given!")
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
    parseInt(name) flatMap { statusCode =>
      val description = Option(response.getDescription)
      val headers = parseHeaders(response.getHeaders)

      for {
        h <- headers
      } yield (statusCode, EndpointResponse(statusCode, description, h))
    }
  }

  def parseHeaders(headers: JMap[String, Header]): Result[Map[String, ResponseHeader]] =
    parseHeaders(Option(headers).map(_.asScala.toMap).getOrElse(Map.empty))

  def parseHeaders(headers: Map[String, Header]): Result[Map[String, ResponseHeader]] = {
    Ok(headers.toSeq
      .map((parseHeader _).tupled)
      .toMap)
  }

  def parseHeader(name: String, header: Header): (String, ResponseHeader) = {
    val desc = Option(header.getDescription)
    val required = header.getRequired
    val deprecated = header.getDeprecated

    (name, ResponseHeader(name, desc, required, deprecated))
  }

  def parseInt(s: String): Result[Int] = {
    try {
      Ok(Integer.parseInt(s))
    } catch {
      case e: NumberFormatException => Error(e.getLocalizedMessage)
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

  def parseSchemas(spec: SpecSource): Result[Seq[ObjectSchema]] =
    Result.sequence(spec.schemas.map((parseSchema(spec.scope) _).tupled).toSeq)

  def parseSchema(origin: Origin)(name: String, schema: Schema[_]): Result[ObjectSchema] = {
    if (schema.getNot != null) Error("Please go a way with your not!")
    else schema match {
      case allOf: ComposedSchema if allOf.getAllOf != null =>
        Ok(ProductSchema(origin / name, Nil))
      case oneOf: ComposedSchema if oneOf.getOneOf != null =>
        Ok(SumSchema(origin / name, Nil))
      case anyOf: ComposedSchema if anyOf.getAnyOf != null =>
        Ok(SumSchema(origin / name, Nil))
      case _ =>
        Ok(SimpleSchema(origin / name))
    }
  }

  def processSpecs(basePath: Option[String], specPaths: Seq[String]): Result[Unit] = {

    def applyAll[T](specs: Seq[SpecSource], f: SpecSource => Result[T]): Result[Seq[T]] =
      Result.sequence(specs.map(f))

    Result.sequence(specPaths.map(SpecSource.load(_: String, basePath))) match {
      case Ok(specs) =>

        applyAll(specs, s => parseEndpoints(s.oai.getPaths))  match {
          case Ok(endpoints) => endpoints.foreach(println)
          case Error(errors) => println(errors)
        }

        val Ok(headers) = applyAll(specs, s => parseHeaders(s.headers)).map(_.flatten.toMap)
        println(s"headers: $headers")

        val Ok(requestBodies) = applyAll(specs, s => Ok(s.requestBodies))
        println(s"request bodies: $requestBodies")

        val Ok(schemas) = applyAll(specs, s => parseSchemas(s)).map(_.flatten)
        schemas.foreach(println)
        Ok(())
      case Error(errors) =>
        Error(errors)
    }
  }

  def main(args: Array[String]): Unit = {

    args match {
      case _ if args.length < 2 =>
        System.err.println("Usage: <base dir> <spec file> [additional spec files...]")
        System.exit(1)
      case Array(base, specs @ _*) =>
        processSpecs(Some(base), specs) match {
          case Ok(_) =>
          case Error(errors) =>
            System.err.println("Errors:")
            errors.foreach(System.err.println)
            System.exit(2)
        }
    }
  }
}
