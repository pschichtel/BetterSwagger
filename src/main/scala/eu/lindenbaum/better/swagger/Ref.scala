package eu.lindenbaum.better.swagger


sealed trait Ref {
  def path: String
}
object Ref {
  def apply(s: String, origin: Option[Source]): Result[Ref] = {
    splitReference(s) match {
      case (source, "") => Error(s"Reference had source, but now path: $source")
      case ("", path) => Ok(LocalRef(path))
      case (source, path) => Source(source, origin).map(s => RemoteRef(s, path))
    }
  }
}
final case class LocalRef(path: String) extends Ref
final case class RemoteRef(source: Source, path: String) extends Ref
