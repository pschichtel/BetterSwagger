package eu.lindenbaum.better.swagger

sealed trait ParameterKind {
  def name: String
}
object ParameterKind {
  def apply(name: String): Result[ParameterKind] = {
    name match {
      case "query" => Ok(Query(name))
      case "header" => Ok(Header(name))
      case "path" => Ok(Path(name))
      case "cookie" => Ok(Cookie(name))
      case in => Error(s"Unknown parameter kind $in")
    }
  }

  final case class Path(name: String) extends ParameterKind
  final case class Query(name: String) extends ParameterKind
  final case class Header(name: String) extends ParameterKind
  final case class Cookie(name: String) extends ParameterKind
}