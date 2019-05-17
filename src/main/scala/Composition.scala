// Composing functions encourages programmers to think about
// what input/output types their functions hold.
// It also allows a programmer to reduce inefficiency that
// can be caused by separately mapping each function.

trait DoubleThreeOps {
  def addThree(a: Double): Double = a + 3
  def multiplyThree(a: Double): Double = a * 3
  def subtractThree(a: Double): Double = a - 3
  def divideThree(a: Double): Double = a / 3
}

// for map
import cats.Functor
// without this we have to do the following: `Functor[F].map(a)(addThree)`
import cats.implicits.toFunctorOps

trait Compose extends DoubleThreeOps {
  // I know. A lot of new things.
  // We have here, a higher kinded type and a context bound.
  // I promise I'll explain these in the future.
  def composed[F[_]: Functor](a: F[Double]): F[Double]
}

object UntaredCompositionInefficient extends Compose {
  def composed[F[_]: Functor](a: F[Double]): F[Double] = {
    // this is inefficient (x4 what it needs to be),
    // especially if F (as a collection) has a large number of elements.
    a.map(addThree).map(multiplyThree).map(subtractThree).map(divideThree)
  }
}

object UntaredCompositionHardToRead extends Compose {
  def compose(a: Double) = divideThree(subtractThree(multiplyThree(addThree(a))))

  def composed[F[_]: Functor](a: F[Double]): F[Double] = {
    a.map(compose)
  }
}

object TaredComposition extends Compose {
  // This is a function. Functions are first class citizens in Scala
  val compose: Double => Double = addThree _ andThen multiplyThree _ andThen subtractThree _ andThen divideThree _

  def composed[F[_] : Functor](a: F[Double]): F[Double] = {
    a.map(compose)
  }
}
