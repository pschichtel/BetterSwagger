package eu.lindenbaum.better

import scala.annotation.tailrec

package object swagger {

  type Result[T] = Either[Seq[String], T]

  object Result {
    def sequence[T](results: Seq[Result[T]]): Result[Seq[T]] = {

      @tailrec
      def loop(results: Seq[Result[T]], values: Seq[T], errors: Seq[String]): Result[Seq[T]] = {
        results match {
          case Seq() if errors.isEmpty => Right(values)
          case Seq() => Left(errors)
          case Seq(Right(value), rest @ _*) => loop(rest, values :+ value, errors)
          case Seq(Left(err), rest @ _*) => loop(rest, values, errors ++ err)
        }
      }

      loop(results, Vector.empty, Vector.empty)
    }

    def required[T](v: T, errStr: String): Result[T] =
      if (v == null) Error(errStr)
      else Ok(v)
  }

  object Ok {
    def apply[T](v: T): Result[T] = Right(v)

    def unapply[T](r: Result[T]): Option[T] = r match {
      case Right(v) => Some(v)
      case _ => None
    }
  }

  object Error {
    def apply[T](err: String): Result[T] = apply(Seq(err))
    def apply[T](err: Seq[String]): Result[T] = Left(err)

    def unapply[T](r: Result[T]): Option[Seq[String]] = r match {
      case Left(errors) => Some(errors)
      case _ => None
    }
  }

  def splitReference(ref: String): (String, String) = {
    val jsonPathSeparator = ref.indexOf('#')
    if (jsonPathSeparator == -1) ("", ref)
    else (ref.substring(0, jsonPathSeparator), ref.substring(jsonPathSeparator + 1))
  }

}
