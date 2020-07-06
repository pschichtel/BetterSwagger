package eu.lindenbaum.better.swagger

sealed trait Origin {
  def / (scopeName: String): Origin = Scope(scopeName, this)

  def source: Option[SourceFile] = this match {
    case Root => None
    case s: SourceFile => Some(s)
    case Scope(_, parent) => parent.source
  }
}

case object Root extends Origin
final case class SourceFile(file: Source, origin: Origin) extends Origin {
  override def toString: String = s"$origin -> <${file.value}>"
}
final case class Scope(name: String, origin: Origin) extends Origin {
  override def toString: String = s"""$origin -> "$name""""
}