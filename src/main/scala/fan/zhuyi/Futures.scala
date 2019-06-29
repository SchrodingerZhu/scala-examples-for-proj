package fan.zhuyi

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.implicitConversions
import cats.Eval
import cats.instances.all._
import cats.syntax.all._
object DeferedFactorial {
  private def cal(n: BigInt) : Eval[BigInt] =
    if (n == 0) Eval.now(1) else Eval.defer(cal(n - 1).map(_ * n))

  def factorial(n: BigInt) : BigInt = cal(n).value
}

object Futures {
  def firstCall : Future[BigInt] = Future(DeferedFactorial.factorial(20000))
  def secondCall : Future[BigInt] = Future(DeferedFactorial.factorial(20000))

  def run : Future[BigInt] = for {
    x <- firstCall
    y <- secondCall
  } yield x + y
}
