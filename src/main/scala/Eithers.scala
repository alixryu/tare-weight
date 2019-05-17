// Why is this my first topic?
// One of the aspects that make Scala a great language for production software is type safety.
// Most programmers that have used static typed languages are used to this benefit.
// However, not all statically typed languages provide a way to handle errors in an identical type safe manner to success cases.
// Eithers provide such a benefit.

trait Types {
  // Define desired types for this program
  type Input = String
  type Output = Double
}

object UntaredConversion extends Types {
  /**
    * @throws java.lang.NumberFormatException if the string does not contain a parsable `Int`.
    * @throws java.lang.NullPointerException  If the string is null.
    */
  def stringToDouble(input: String): Double = input.toDouble

  /**
    * @throws java.lang.ArithmeticException if an an integer is divided by zero.
    */
  def divideBy(inputA: Double)(inputB: Double): Double = inputA / inputB

  def fullConversion(input: Input): Output = {
    // using function composition when possible
    val fullConv = stringToDouble _ andThen divideBy(0) _

    fullConv(input)
  }
}

object TaredConversion extends Types {
  // for better Either syntax
  import cats.syntax.either._


  // ADT (Algebraic Data Types) for Errors of this program
  trait GeneralConversionException extends Throwable
  case class ToDoubleException() extends GeneralConversionException
  case class DivideByZeroException() extends GeneralConversionException

  // tared output type
  type TaredOutput = Either[GeneralConversionException, Output]

  def stringToDouble(input: String): Either[ToDoubleException, Double] = {
    Either.catchNonFatal[Double](input.toDouble)
      .leftMap(_ => ToDoubleException())
  }

  // TODO: hold on this doesn't throw. It only does when input is Int Find different example, haha.
  def divideBy(inputA: Double)(inputB: Double): Either[DivideByZeroException, Double] = {
    Either.catchOnly[ArithmeticException](inputA / inputB)
      .leftMap(_ => DivideByZeroException())
  }

  def fullConversion(input: Input): TaredOutput = {
    // flatMap on Eithers
    for {
      d <- stringToDouble(input)
      r <- divideBy(0)(d)
    } yield r
  }
}
