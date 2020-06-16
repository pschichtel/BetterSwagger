package eu.lindenbaum.better.swagger

sealed trait ParameterKind {
  def name: String
}
object ParameterKind {
  def apply(name: String): Result[ParameterKind] = {
    name match {
      case "query" => Result.ok(Query(name))
      case "header" => Result.ok(Header(name))
      case "path" => Result.ok(Path(name))
      case "cookie" => Result.ok(Cookie(name))
      case in => Result.error(s"Unknown parameter location $in")
    }
  }

  final case class Path(name: String) extends ParameterKind
  final case class Query(name: String) extends ParameterKind
  final case class Header(name: String) extends ParameterKind
  final case class Cookie(name: String) extends ParameterKind
  final case class Body(name: String) extends ParameterKind
}