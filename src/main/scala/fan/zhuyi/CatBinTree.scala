package fan.zhuyi
import cats.Order
import cats.Comparison._
import cats.Functor
import cats.syntax.functor._
import scala.language.higherKinds

import scala.language.implicitConversions

sealed trait CatBinTree[+A] {
  def insert[U >: A](x: U)(implicit order: Order[U]) : CatBinTree[U] = this match {
    case Empty => Node(Empty, x, Empty)
    case Node(l, y, r) => order.comparison(x, y) match {
      case LessThan => Node(l.insert(x), y, r)
      case EqualTo => Node(l, y, r)
      case GreaterThan => Node(l, y, r.insert(x))
    }
  }

  def contains[U >: A](x: U)(implicit order: Order[U]) : Boolean = this match {
    case Empty => false
    case Node(l, y, r) => order.comparison(x, y) match {
      case LessThan => l.contains(x)
      case EqualTo => true
      case GreaterThan => r.contains(x)
    }
  }

  override def toString: String = this match {
    case Empty => "<E>"
    case Node(l, x, r) => "[%s, %s, %s]".format(l.toString, x.toString, r.toString)
  }
}
final case class Node[+A](l: CatBinTree[A], x: A, r: CatBinTree[A]) extends CatBinTree[A]
final case object Empty extends CatBinTree[Nothing]

object CatBinTree {
  implicit val treeFunctor : Functor[CatBinTree] = new Functor[CatBinTree] {
    override def map[A, B](fa: CatBinTree[A])(f: A => B): CatBinTree[B] = fa match {
      case Empty => Empty
      case Node(l, x, r) => Node(map(l)(f), f(x), map(r)(f))
    }
  }
}

