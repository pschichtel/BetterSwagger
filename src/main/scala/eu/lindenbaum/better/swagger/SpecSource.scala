package eu.lindenbaum.better.swagger

import java.net.URL
import java.nio.file.{Path, Paths}

import io.swagger.v3.oas.models.OpenAPI
import io.swagger.v3.parser.OpenAPIV3Parser

import scala.util.{Success, Try}

case class SpecSource(ref: String, oai: OpenAPI)

object SpecSource {

  def load(ref: String, origin: Option[String]): Result[SpecSource] = {

    def read(s: String) = {
      Option(new OpenAPIV3Parser().read(s)) match {
        case Some(oai) => Ok(SpecSource(s, oai))
        case _ => Error(s"Failed to read referenced spec: $s")
      }
    }

    def parseRef(ref: String): Either[URL, Path] = {
      Try(new URL(ref)) match {
        case Success(url) =>
          Left(url)
        case _ =>
          Right(Paths.get(ref))
      }
    }

    val (filePart, _) = splitReference(ref)

    if (filePart.isEmpty) {
      origin match {
        case Some(ref) => read(ref)
        case None => Error("Cannot read spec for internal reference without an origin")
      }
    } else parseRef(filePart) match {
      case Left(url) =>
        // given ref is a valid URL and as such absolute, so we're done here
        read(url.toExternalForm)
      case Right(path) =>
        read(path.toString).orElse {
          origin.map(parseRef) match {
            case Some(Left(baseUrl)) =>
              read(baseUrl.toURI.resolve(path.toString).toURL.toExternalForm)
            case Some(Right(basePath)) =>
              read(basePath.resolve(path).toRealPath().toString)
            case None =>
              read(path.toString)
          }
        }
    }
  }
}
