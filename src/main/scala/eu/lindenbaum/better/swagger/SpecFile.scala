package eu.lindenbaum.better.swagger

import java.net.URL
import java.nio.file.{Path, Paths}
import java.util.Collections

import io.swagger.v3.oas.models.headers.Header
import io.swagger.v3.oas.models.media.Schema
import io.swagger.v3.oas.models.parameters.RequestBody
import io.swagger.v3.oas.models.{Components, OpenAPI}
import io.swagger.v3.parser.OpenAPIV3Parser
import io.swagger.v3.parser.core.models.ParseOptions

import scala.jdk.CollectionConverters._
import scala.util.{Success, Try}

case class SpecFile(oai: OpenAPI, scope: SourceFile) {
  def components: Option[Components] = Option(oai.getComponents)

  def schemas: Map[String, Schema[_]] = components.flatMap(c => Option(c.getSchemas).map(_.asScala.toMap)).getOrElse(Map.empty)
  def headers: Map[String, Header] = components.flatMap(c => Option(c.getHeaders).map(_.asScala.toMap)).getOrElse(Map.empty)
  def requestBodies: Map[String, RequestBody] = components.flatMap(c => Option(c.getRequestBodies).map(_.asScala.toMap)).getOrElse(Map.empty)
}


object SpecFile {

  val ParserOptions: ParseOptions = {
    val options = new ParseOptions()
    options.setCamelCaseFlattenNaming(false)
    options.setResolve(false)
    options.setResolveCombinators(false)
    options.setResolveFully(false)
    options.setFlatten(false)
    options.setSkipMatches(false)
    options.setFlattenComposedSchemas(false)
    options
  }

  private def read(s: Source) = {
    Option(new OpenAPIV3Parser().read(s.value, Collections.emptyList(), ParserOptions)) match {
      case Some(oai) => Ok(SpecFile(oai, SourceFile(s, Root)))
      case _         => Error(s"Failed to read referenced spec: $s")
    }
  }

  private def parseRef(ref: String): Either[URL, Path] = {
    Try(new URL(ref)) match {
      case Success(url) => Left(url)
      case _            => Right(Paths.get(ref))
    }
  }

  def load(r: String, origin: Option[Source]): Result[SpecFile] =
    Source(r, origin).flatMap(read)
}
