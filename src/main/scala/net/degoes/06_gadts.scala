package net.degoes

/*
 * INTRODUCTION
 *
 * In Functional Design, type safety of data models can be greatly improved by
 * using so-called generalized algebraic data types.
 *
 * In this section, you'll review GADTs with a focus on functional models.
 *
 */

object Motivation {
  sealed trait Expr[A] { self =>
    def +(that: Expr[A])(implicit aIsInt: A <:< Int): Expr[Int] =
      Expr.Add(self.as[Int], that.as[Int])

    def as[B](implicit ev: A <:< B): Expr[B] = Expr.As(self, ev)
  }
  object Expr {
    final case class ConstantInt(value: Int)                extends Expr[Int]
    final case class ConstantStr(value: String)             extends Expr[String]
    final case class As[A, B](expr: Expr[A], ev: A <:< B)   extends Expr[B]
    final case class Add(left: Expr[Int], right: Expr[Int]) extends Expr[Int]

    def int(v: Int): Expr[Int] = ConstantInt(v)

    def str(v: String): Expr[String] = ConstantStr(v)
  }

  def eval[A](expr: Expr[A]): A =
    expr match {
      case Expr.Add(left, right) => eval(left) + eval(right)

      case Expr.ConstantInt(v) => v

      case Expr.ConstantStr(v) => v

      case Expr.As(expr, ev) => ev(eval(expr))
    }

  // eval(Expr.str("foo") + Expr.str("bar"))
}

object Motivation2 {
  val any2stringadd = null
  sealed trait Expr[A] { self =>
  }
  object Expr {

    // This is another strategy for adding the operators as extension methods
    implicit class ExprInt(val self: Expr[Int]) extends AnyVal {
      def +(that: Expr[Int]): Expr[Int] = Expr.Add(self, that)
    }

    final case class ConstantInt(value: Int)                extends Expr[Int]
    final case class ConstantStr(value: String)             extends Expr[String]
    final case class Add(left: Expr[Int], right: Expr[Int]) extends Expr[Int]

    def int(v: Int): Expr[Int] = ConstantInt(v)

    def str(v: String): Expr[String] = ConstantStr(v)
  }

  def eval[A](expr: Expr[A]): A =
    expr match {
      case Expr.Add(left, right) => eval(left) + eval(right)

      case Expr.ConstantInt(v) => v

      case Expr.ConstantStr(v) => v
    }

  eval(Expr.int(22) + Expr.int(20))
}

/**
 * EXPRESSIONS - EXERCISE SET 1
 *
 * Consider an application (such as the spreadsheet example) that needs to
 * calculate values in a user-defined way.
 */
object expr {
  sealed trait CalculatedValue[+A] { self =>

  }
  object CalculatedValue {
    implicit class CalculatedIntValue(val self: CalculatedValue[Int]) extends AnyVal {
      def add(that: CalculatedValue[Int]): CalculatedValue[Int] = Add(self, that)
    }
    final case class Integer(value: Int) extends CalculatedValue[Int]
    final case class Str(value: String)  extends CalculatedValue[String]

    /**
     * EXERCISE 1
     *
     * Add an operator that adds two integer expressions, yielding an integer
     * expression.
     *
     * NOTE: Be sure to modify the `calculate` method below, so that it can
     * handle the new operation.
     */
    final case class Add(left: CalculatedValue[Int], right: CalculatedValue[Int]) extends CalculatedValue[Int]

    /**
     * EXERCISE 2
     *
     * Add an operator that subtracts an integer from another integer expression,
     * yielding an integer expression.
     *
     * NOTE: Be sure to modify the `calculate` method below, so that it can
     * handle the new operation.
     */
    final case class Subtract(left: CalculatedValue[Int], right: CalculatedValue[Int]) extends CalculatedValue[Int]

    /**
     * EXERCISE 3
     *
     * Add an operator that multiplies two integer expressions, yielding an
     * integer expression.
     *
     * NOTE: Be sure to modify the `calculate` method below, so that it can
     * handle the new operation.
     */
    final case class Multiply(left: CalculatedValue[Int], right: CalculatedValue[Int]) extends CalculatedValue[Int]

    /**
     * EXERCISE 4
     *
     * Add an operator that concatenates two strings, yielding a string
     * expression.
     *
     * NOTE: Be sure to modify the `calculate` method below, so that it can
     * handle the new operation.
     */
    final case class Concat(left: CalculatedValue[String], right: CalculatedValue[String])
        extends CalculatedValue[String]

    /**
     * EXERCISE 5
     *
     * Add an operator that determines if a string starts with a specified
     * prefix, yielding a boolean expression.
     *
     * NOTE: Be sure to modify the `calculate` method below, so that it can
     * handle the new operation.
     */
    final case class StartsWith(prefix: CalculatedValue[String], value: CalculatedValue[String])
        extends CalculatedValue[Boolean]
  }

  import CalculatedValue._

  def calculate[A](expr: CalculatedValue[A]): A =
    expr match {
      case Integer(v)                => v
      case Str(v)                    => v
      case Add(left, right)          => calculate(left) + calculate(right)
      case Subtract(left, right)     => calculate(left) - calculate(right)
      case Multiply(left, right)     => calculate(left) * calculate(right)
      case Concat(left, right)       => calculate(left) + calculate(right)
      case StartsWith(prefix, value) => calculate(value).startsWith(calculate(prefix))
    }

  import zio.UIO
  def calculateM[A](expr: CalculatedValue[A]): UIO[A] =
    UIO.effectSuspendTotal {
      expr match {
        case Integer(value)          => UIO(value)
        case Str(value)              => UIO(value)
        case Add(left, right)        => (calculateM(left) zipWith calculateM(right))(_ + _)
        case Subtract(left, right)   => (calculateM(left) zipWith calculateM(right))(_ - _)
        case Multiply(left, right)   => (calculateM(left) zipWith calculateM(right))(_ * _)
        case Concat(left, right)     => (calculateM(left) zipWith calculateM(right))(_ + _)
        case StartsWith(left, right) => (calculateM(left) zipWith calculateM(right))(_ startsWith _)
      }
    }
}

object InterpreterSideBar {
  import zio.UIO
  import expr.CalculatedValue

  type Interpreter[F[_], A, G[_], B] = F[A] => G[A]

  trait Evaluator[F[_], G[_]] {
    def apply[A](fa: F[A]): G[A]
  }

  type PrettyPrinter[A] = Interpreter[CalculatedValue, A, UIO, String]
}

/**
 * PARSERS - EXERCISE SET 2
 */
object parser {
  sealed trait Parser[+A]
  object Parser {
    final case object OneChar extends Parser[Char]

    /**
     * EXERCISE 1
     *
     * Add an operator that can repeat a parser between some lower range
     * (optional) and some upper range (optional).
     *
     * NOTE: Be sure to modify the `parse` method below, so that it can
     * handle the new operation.
     */
    final case class Repeat[A](min: Option[Int], max: Option[Int], parser: () => Parser[A]) extends Parser[List[A]]

    /**
     * EXERCISE 2
     *
     * Add a constructor that models the production of the specified value (of
     * any type at all), without consuming any input.
     *
     * NOTE: Be sure to modify the `parse` method below, so that it can
     * handle the new operation.
     */
    final case class Succeed[A](value: A) extends Parser[A]

    /**
     * EXERCISE 3
     *
     * Add a constructor that models failure with a string error message.
     *
     * NOTE: Be sure to modify the `parse` method below, so that it can
     * handle the new operation.
     */
    final case class Fail(value: String) extends Parser[Nothing]

    /**
     * EXERCISE 4
     *
     * Add an operator that can try one parser, but if that fails, try
     * another parser.
     *
     * NOTE: Be sure to modify the `parse` method below, so that it can
     * handle the new operation.
     */
    final case class OrElse[A, B](left: () => Parser[A], right: () => Parser[B]) extends Parser[Either[A, B]]

    /**
     * EXERCISE 5
     *
     * Add an operator that parses one thing, and then parses another one,
     * in sequence, producing a tuple of their results.
     *
     * NOTE: Be sure to modify the `parse` method below, so that it can
     * handle the new operation.
     */
    final case class Sequence[A, B](left: () => Parser[A], right: () => Parser[B]) extends Parser[(A, B)]
  }

  import Parser._

  def parse[A](parser: Parser[A], input: String): Either[String, (String, A)] =
    parser match {
      case OneChar =>
        input.headOption
          .map((a: Char) => Right(input.drop(1) -> a))
          .getOrElse(Left("The input to the parser has no remaining characters"))
      case Repeat(min0, max0, parser) =>
        val min = min0.getOrElse(0)
        val max = max0.getOrElse(Int.MaxValue)
        parse(parser(), input) match {
          case Left(error) => if (min > 0) Left(error) else Right((input, Nil))
          case Right((input, value)) =>
            parse(Repeat(Some(min - 1), Some(max), parser), input) match {
              case Left(error)          => if (min > 1) Left(error) else Right((input, value :: Nil))
              case Right((input, tail)) => Right((input, value :: tail))
            }
        }

      case Succeed(value) => Right(input -> value)
      case Fail(message)  => Left(message)
      case OrElse(left, right) =>
        parse(left(), input).map { case (input, a) => (input, Left(a)) } match {
          case Left(error) =>
            parse(right(), input).map { case (input, b) => (input, Right(b)) }
          case either => either
        }

      case Sequence(left, right) =>
        parse(left(), input).flatMap {
          case (input, a) =>
            parse(right(), input).map {
              case (input, b) => ((input, (a, b)))
            }
        }
    }
}
