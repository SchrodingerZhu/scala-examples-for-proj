package fan.zhuyi

import scala.math.sqrt
sealed trait Expr {
  def eval() : Either[String, Double] = this match {
    case Add(lhs, rhs) => for {
      x <- lhs.eval()
      y <- rhs.eval()
    } yield x + y
    case Sub(lhs, rhs) => for {
      x <- lhs.eval()
      y <- rhs.eval()
    } yield x - y
    case Div(lhs, rhs) => for {
      x <- lhs.eval()
      y <- rhs.eval()
      z <- if (y != 0.toDouble) Right(x / y) else Left("zero division")
    } yield z
    case Sqrt(operand) => for {
      x <- operand.eval()
      y <- if (x >= 0) Right(sqrt(x)) else Left("invalid sqrt")
    } yield sqrt(x)
    case Num(x) => Right(x)
  }
}

final case class Add(lhs: Expr, rhs: Expr) extends Expr
final case class Sub(lhs: Expr, rhs: Expr) extends Expr
final case class Div(lhs: Expr, rhs: Expr) extends Expr
final case class Sqrt(operand: Expr) extends Expr
final case class Num(x: Double) extends Expr