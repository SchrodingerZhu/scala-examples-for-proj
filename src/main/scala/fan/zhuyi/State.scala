package fan.zhuyi
import cats.instances.all._
import cats.syntax.all._
import cats.data.State
import cats.data.State._

object StateTest {
  def count[A](l : List[A]) : State[Int, Int] = {
    l match {
      case List() =>
        State.get[Int]
      case _ :: tail =>
        for{
          _ <- modify[Int](_ + 1)
          x <- count(tail)
        } yield x
    }
  }
}
