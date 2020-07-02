package eu.lindenbaum.better.swagger

import java.net.URL
import java.nio.file.{Files, Path, Paths}

import scala.util.{Failure, Success, Try}

sealed trait Source {
  def value: String
}
object Source {
  private def makeReal(p: Path): Result[Path] = {
    Try(p.toRealPath()) match {
      case Success(realPath) => Ok(realPath)
      case Failure(ex) => Error(s"Failed to detect real path: ${ex.getLocalizedMessage}")
    }
  }

  private def fromPath(p: Path): Result[FileSource] = {
    if (Files.exists(p)) makeReal(p).map(real => FileSource(real))
    else Error("File does not exist")
  }

  def toUrl(s: String): Option[URL] = Try(new URL(s)).toOption

  def apply(value: String): Result[Source] = apply(value, None)

  def apply(value: String, base: Option[Source]): Result[Source] = {
    toUrl(value) match {
      case Some(url) => Ok(UrlSource(url))
      case None =>
        val path = Paths.get(value)
        if (path.isAbsolute || Files.exists(path)) fromPath(path)
        else {
          base match {
            case Some(FileSource(basePath)) => fromPath(basePath.resolveSibling(path))
            case Some(UrlSource(url)) => Ok(UrlSource(url.toURI.resolve(value).toURL))
            case _ => fromPath(path)
          }
        }
    }
  }
}
final case class UrlSource(url: URL) extends Source {
  override def value: String = url.toExternalForm
}
final case class FileSource(path: Path) extends Source {
  override def value: String = path.toString
}