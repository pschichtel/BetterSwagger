package eu.lindenbaum.better.swagger

import io.swagger.v3.oas.models.headers.Header
import io.swagger.v3.oas.models.parameters.{Parameter, RequestBody}
import io.swagger.v3.oas.models.responses.ApiResponse
import io.swagger.v3.oas.models.{OpenAPI, Operation, PathItem, Paths}
import java.util.{List => JList, Map => JMap}

import io.swagger.v3.oas.models.media.{ArraySchema, ComposedSchema, Schema}

import scala.jdk.CollectionConverters._

object Main {

  def parseEndpoints(origin: Origin, paths: Paths): Result[Seq[Endpoint]] = {
    val endpoints = Option(paths)
      .map(_.asScala.toSeq)
      .getOrElse(Seq.empty)
      .map { case (name, path) => parsePath(origin / name, name, path) }

    Result.sequence(endpoints).map(_.flatten)
  }

  def parsePath(origin: Origin, path: String, item: PathItem): Result[Seq[Endpoint]] = {

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

    Result.sequence(for ((method, op) <- ops) yield parseOperation(origin / method / op.getOperationId, method, path, op))
  }

  def parseOperation(origin: Origin, method: String, path: String, op: Operation): Result[Endpoint] = {
    val desc = Option(op.getDescription)
    val summary = Option(op.getSummary)
    val id = op.getOperationId
    if (id == null) {
      Error("No operationId given!")
    } else {
      val tags = Option(op.getTags).map(_.asScala.toSet).getOrElse(Set.empty)

      for {
        params <- parseParameters(origin / "params", op)
        bodies <- parseRequestBody(origin / "body", op.getRequestBody)
        responses <- parseResponses(origin / "responses", op)
      } yield Endpoint(id, method, path, summary, desc, tags, params, bodies, responses)
    }
  }

  def parseParameters(origin: Origin, op: Operation): Result[Seq[EndpointParameter]] = {
    val params = Option(op.getParameters)
      .map(_.asScala.toSeq)
      .getOrElse(Nil)
      .map(p => parseParameter(origin, p))
    Result.sequence(params)
  }

  def parseParameter(origin: Origin, p: Parameter): Result[EndpointParameter] = {
    val description = Option(p.getDescription)
    val required = p.getRequired
    val deprecated = p.getDeprecated
    val explode = p.getExplode
    val allowReserved = p.getAllowReserved
    val allowEmptyValue = p.getAllowEmptyValue

    for {
      name <- Result.required(p.getName, "name is required")
      kind <- ParameterKind(p.getIn)
      schema <- parseSchema(origin / name, p.getSchema)
    } yield EndpointParameter(name, kind, description, required, deprecated, explode, allowReserved, allowEmptyValue, schema)
  }

  def parseResponses(origin: Origin, op: Operation): Result[Map[Int, Map[String, EndpointResponse]]] = {
    val responses = Option(op.getResponses)
      .map(_.asScala.toSeq)
      .getOrElse(Seq.empty)
      .map { case (name, response) => parseResponse(origin / name, name, response) }

    Result.sequence(responses).map(_.toMap)
  }

  def parseResponse(origin: Origin, name: String, response: ApiResponse): Result[(Int, Map[String, EndpointResponse])] = {
    for {
      statusCode <- parseInt(name)
      description = Option(response.getDescription)
      headers <- parseHeaders(origin / "headers", response.getHeaders)
      contents = Option(response.getContent)
        .map(_.asScala.toSeq)
        .getOrElse(Seq.empty)
        .map { case (mediaType, content) =>
          for {
            schema <- parseSchema(origin / name / mediaType, content.getSchema)
          } yield (mediaType, EndpointResponse(statusCode, mediaType, description, headers, schema))
        }
      c <- Result.sequence(contents)
    } yield (statusCode, c.toMap)
  }

  def parseHeaders(origin: Origin, headers: JMap[String, Header]): Result[Map[String, ResponseHeader]] =
    parseHeaders(origin, Option(headers).map(_.asScala.toMap).getOrElse(Map.empty))

  def parseHeaders(origin: Origin, headers: Map[String, Header]): Result[Map[String, ResponseHeader]] = {
    val parsed = headers.toSeq
      .map { case (name, header) => parseHeader(origin / name, name, header) }
    Result.sequence(parsed).map(_.toMap)
  }

  def parseHeader(origin: Origin, name: String, header: Header): Result[(String, ResponseHeader)] = {
    val desc = Option(header.getDescription)
    val required = header.getRequired
    val deprecated = header.getDeprecated
    for {
      schema <- parseSchema(origin, header.getSchema)
    } yield (name, ResponseHeader(name, desc, required, deprecated, schema))
  }

  def parseInt(s: String): Result[Int] = {
    try {
      Ok(Integer.parseInt(s))
    } catch {
      case e: NumberFormatException => Error(s"Failed to parse number: ${e.getLocalizedMessage}")
    }
  }

  def parseRequestBodies(origin: Origin, openAPI: OpenAPI): Result[Map[String, Map[String, EndpointBody]]] = {
    val bodies = Option(openAPI.getComponents)
      .flatMap(c => Option(c.getRequestBodies))
      .map(_.asScala.toSeq)
      .getOrElse(Seq.empty)
      .map { case (name, body) => parseRequestBody(origin / name, body).map(r => (name, r)) }

    Result.sequence(bodies).map(_.toMap)
  }

  def parseRequestBody(origin: Origin, body: RequestBody): Result[Map[String, EndpointBody]] = {
    if (body == null) Ok(Map.empty)
    else {
      val description = Option(body.getDescription)
      val bodies = Option(body.getContent)
        .map(_.asScala.toSeq)
        .getOrElse(Seq.empty)
        .map { case (mediaType, content) =>
          for {
            schema <- parseSchema(origin / mediaType, content.getSchema)
          } yield (mediaType, EndpointBody(mediaType, description, schema))
        }

      Result.sequence(bodies).map(_.toMap)
    }
  }

  def parseSchemas(spec: SpecFile): Result[Seq[ObjectSchema]] =
    Result.sequence(spec.schemas.map(s => parseSchema(spec.scope / "schemas" / s._1, s._2)).toSeq)

  def parseSchemaMetadata(name: Option[String], schema: Schema[_]): Result[ObjectMetadata] = {
    val title = Option(schema.getTitle)
    val metadataName = name.orElse(Option(schema.getName))
    val description = Option(schema.getDescription)
    val deprecated = Option(schema.getDeprecated).exists(_.booleanValue())
    val requiredProperties = Option(schema.getRequired).map(_.asScala.toSeq).getOrElse(Seq.empty)
    for {
      default <- parseDefaultValue(schema)
    } yield ObjectMetadata(metadataName, title, description, deprecated, default, requiredProperties)
  }

  def parseSchemaType(schema: Schema[_]): SchemaType[_] = {
    val format = Option(schema.getFormat)
    val typeName = schema.getTitle
    Option(typeName)
      .map(_.toLowerCase.trim)
      .map {
        case "" => SchemaType.Unspecified(format)
        case "string" => SchemaType.Str(format)
        case "number" => SchemaType.Number(format)
        case "integer" => SchemaType.Integer(format)
        case "boolean" => SchemaType.Bool(format)
        case s => SchemaType.Unknown(s, format)
      }
      .getOrElse(SchemaType.Unspecified(format))
  }

  def parseDefaultValue(schema: Schema[_]): Result[Option[Any]] =
    Ok(Option(schema.getDefault).flatMap(DefaultValue.fromAny))

  def parseSchema(origin: Origin, schema: Schema[_]): Result[ObjectSchema] = {

    def readChildren(schemas: JList[Schema[_]]): Result[Seq[ObjectSchema]] = {
      val terms = schemas.asScala.toSeq.zipWithIndex.map { case (s, i) =>
        val productOrigin = origin / s"$i"
        parseSchema(productOrigin, s)
      }
      Result.sequence(terms)
    }

    if (schema.getNot != null) Error("Please go a way with your not!")
    else {
      val schemaType = parseSchemaType(schema)
      parseSchemaMetadata(None, schema).flatMap { metadata =>
        schema match {
          case ref if ref.get$ref() != null =>
            Ref(ref.get$ref(), origin.source.map(_.file)).map(ReferencedSchema(origin, metadata, _: Ref))
          case array: ArraySchema =>
            val itemsSchema = array.getItems
            if (itemsSchema == null) Error("No item schema provided in array schema!")
            else parseSchema(origin / "items", itemsSchema).map { schema =>
              SeqSchema(origin, metadata, schema)
            }
          case composed: ComposedSchema =>
            (composed.getAllOf, composed.getOneOf, composed.getAnyOf) match {
              case (children, null, null) if children.size() > 0 =>
                readChildren(children).map(t => ProductSchema(origin, metadata, t))
              case (null, children, null) if children.size() > 0 =>
                readChildren(children).map(t => SumSchema(origin, metadata, t))
              case (null, null, children) if children.size() > 0 =>
                readChildren(children).map(t => SumSchema(origin, metadata, t))
              case (null, null, null) =>
                Error("composed object found, that does not define any compositions")
              case _ => Error("composed object cannot be composed in in different ways at once, please use nesting!")
            }
          case single if single.getProperties != null =>
            val props = Result.sequence(single.getProperties.asScala.toSeq.map { case (name, schema) =>
              parseSchema(origin / name, schema).map(s => (name, s))
            }).map(_.toMap)
            props.map(p => SingleSchema(origin, metadata, p))
          case enum if enum.getEnum != null =>
            Option(enum.getEnum).map(_.asScala.toList.map(_.toString)).getOrElse(Nil) match {
              case Nil => Error("enum without values is not allowed!")
              case constant :: Nil => Ok(ConstantSchema(origin, schemaType, metadata, constant))
              case values => Ok(EnumSchema(origin, schemaType, metadata, values))
            }
          case primitive if primitive.getType != null =>
            Ok(PrimitiveSchema(origin, schemaType, metadata))
          case _ =>
            Ok(MetadataOnlySchema(origin, schemaType, metadata))
        }
      }
    }
  }

  def processSpecs(basePath: Option[Source], specPaths: Seq[String]): Result[Unit] = {

    def applyAll[T](specs: Seq[SpecFile], f: SpecFile => Result[T]): Result[Seq[T]] =
      Result.sequence(specs.map(f))

    Result.sequence(specPaths.map(SpecFile.load(_: String, basePath))) match {
      case Ok(specs) =>

        applyAll(specs, s => parseEndpoints(s.scope, s.oai.getPaths))  match {
          case Ok(endpoints) => endpoints.foreach(println)
          case Error(errors) => println(errors)
        }

        val Ok(headers) = applyAll(specs, s => parseHeaders(s.scope, s.headers)).map(_.flatten.toMap)
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
        processSpecs(Source(base).toOption, specs) match {
          case Ok(_) =>
          case Error(errors) =>
            System.err.println("Errors:")
            errors.foreach(System.err.println)
            System.exit(2)
        }
    }
  }
}
