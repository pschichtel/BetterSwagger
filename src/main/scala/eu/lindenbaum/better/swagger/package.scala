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
      if (v == null) error(errStr)
      else ok(v)

    def ok[T](v: T): Result[T] = Right(v)
    def error[T](s: String): Result[T] = Left(Seq(s))
  }

}
