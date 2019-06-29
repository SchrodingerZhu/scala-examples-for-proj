package SumType

sealed trait Sum[+A, +B] {
  def flatMap[U >: A, C](f: B => Sum[U, C]) : Sum[U, C] = this match {
    case Failure(v) => Failure(v)
    case Success(v) => f(v)
  }
}
final case class Failure[A, B](value: A) extends Sum[A, Nothing]
final case class Success[A, B](value: B) extends Sum[Nothing, B]

