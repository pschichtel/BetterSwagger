package eu.lindenbaum.better.swagger

import scala.jdk.CollectionConverters._

sealed trait DefaultValue
object DefaultValue {
  final case class Num(value: BigDecimal) extends DefaultValue
  final case class Str(value: String) extends DefaultValue
  final case class Bool(value: Boolean) extends DefaultValue
  final case class Sequence(values: Seq[DefaultValue]) extends DefaultValue
  final case class Object(values: Map[String, DefaultValue]) extends DefaultValue

  def fromAny(v: Any): Option[DefaultValue] = v match {
    case null => None
    case n: java.lang.Long => Some(Num(BigDecimal(n)))
    case n: java.lang.Integer => Some(Num(BigDecimal(n)))
    case n: java.lang.Short => Some(Num(BigDecimal(n.toInt)))
    case n: java.lang.Byte => Some(Num(BigDecimal(n.toInt)))
    case n: java.lang.Double => Some(Num(BigDecimal(n)))
    case n: java.lang.Float => Some(Num(BigDecimal(n.toDouble)))
    case n: java.math.BigInteger => Some(Num(new java.math.BigDecimal(n)))
    case n: java.math.BigDecimal => Some(Num(n))
    case s: String => Some(Str(s))
    case true => Some(Bool(true))
    case false => Some(Bool(false))
    case o: java.util.Map[String, _] @unchecked =>
      Some(Object(o.asScala.toSeq.flatMap { case (name, value) => fromAny(value).map(v => (name, v))}.toMap))
    case i: java.lang.Iterable[_] => Some(Sequence(i.asScala.toSeq.flatMap(fromAny)))
  }
}
