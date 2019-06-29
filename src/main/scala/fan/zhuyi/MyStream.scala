package fan.zhuyi

sealed trait MyStream[+A] {
  def take(n: Int) : List[A] = (n, this) match {
    case (0, _) => List()
    case (_, Nil) => List()
    case (_, x : Cons[A]) =>  {
      x.element +: x.tail.take(n - 1)
    }
  }
}

final class Cons[+A](x : => A, t: => MyStream[A]) extends MyStream[A] {
  lazy val element: A = x
  lazy val tail: MyStream[A] = t
}
final object Nil extends MyStream[Nothing]
object MyStream {
  def repeat[A](x : => A) : MyStream[A] = new Cons(x, repeat(x))
}