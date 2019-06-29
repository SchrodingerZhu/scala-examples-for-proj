import cats.instances.all._
import cats.syntax.all._
import cats.Eval
import fan.zhuyi._
import scala.concurrent.duration._
import scala.concurrent.Await
import scala.language.implicitConversions
import scala.math.Ordered
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}
sealed trait BinTree[+T]
case class Branch[+T](left : BinTree[T], x: T, right: BinTree[T]) extends BinTree[T]
case object Nil extends BinTree[Nothing]

object BinTree {
  def insert[A](x : A, t : BinTree[A])(implicit compare : Ordering[A]): BinTree[A] = t match {
    case Nil => Branch(Nil, x, Nil)
    case Branch(l, y, r) => if (compare.lteq(x, y)) Branch(insert(x, l), y, r) else Branch(l, y, insert(x, r))
  }
}

object Test {
  def factorial(n : BigInt) : BigInt = deferred_factorial(n).value
  def deferred_factorial(n : BigInt) : Eval[BigInt] =
    if (n == 1) Eval.now(n) else Eval.defer(deferred_factorial(n - 1).map(_ * n))
  def ownCmp[A](a : Ordered[A], b : A): Boolean = {
    a < b
  }
}

object Hello extends App {
  var a = Empty.insert(1).insert(3).insert(0).insert(9).insert(-5)
  a = a.map(_ + 1)
  println(a)
  val test = Futures.run
  val words = Words("a", "b", "c", "d")
  val readerTest = ReaderTest.allReader.run(words)
  println(readerTest)
  println(ReaderTest.allReader2.run("test"))
  test.onComplete {
    case Success(result) => println(s"result = $result")
    case Failure(e) => e.printStackTrace
  }
  val test2 = StateTest.count(List(1, 2, 3, 4, 5, 6, 6, 8)).run(0)
  println(test2.value._1)
  Thread.sleep(1000)
}