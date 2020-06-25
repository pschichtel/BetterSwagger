package eu.lindenbaum.better.swagger

case class ObjectSchema(origin: Origin)

sealed trait Origin {
  def / (scopeName: String): Origin = Scope(scopeName, this)
}
object Origin {
  def fromRef(r: String): Option[Origin] = {
    val (_, ref) = splitReference(r)

    if (ref.isEmpty) None
    else {
      val lastSlash = ref.lastIndexOf('/')
      if (lastSlash == -1) None
      else {
        Some(Root / ref.substring(lastSlash + 1))
      }
    }
  }
}
case object Root extends Origin
final case class Scope(name: String, origin: Origin) extends Origin
