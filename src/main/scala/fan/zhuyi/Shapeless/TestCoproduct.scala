package fan.zhuyi.Shapeless

import fan.zhuyi.Shapeless.CsvEncoder._
import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, Lazy}

sealed trait Shape

final case class Rectangle(width: Double, height: Double) extends Shape

final case class Circle(radius: Double) extends Shape

sealed trait Tree[+A]
case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]
case class Leaf[+A](value: A) extends Tree[A]


trait CsvEncoder[A] {
  def encode(value: A): List[String]
}

object CsvEncoder extends {
  def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] = enc

  def instance[A](func: A => List[String]): CsvEncoder[A] = {
    value: A => {
      func(value)
    }
  }

  implicit class CsvWriter[T](value: T)(implicit enc: CsvEncoder[T]) {
    def toCSV: String = enc.encode(value).mkString(",")
  }


  //implicit val employeeEncoder: CsvEncoder[Employee] = {
  //  value: Employee =>
  //    List(
  //      value.name,
  //      value.number.toString,
  //      if (value.manager) "yes" else "no"
  //    )
  //}

  def writeCsv[A](values: List[A])(implicit enc: CsvEncoder[A]): String = {
    values.map(enc.encode(_).mkString(",")).mkString("\n")
  }

  val employee: List[Employee] = List(
    Employee("Bill", 1, manager = true),
    Employee("Peter", 2, manager = false),
    Employee("Milton", 3, manager = false)
  )


  //implicit val iceCreamEncoder: CsvEncoder[IceCream] = {
  //  value: IceCream =>
  //    List(
  //      value.name,
  //      value.numCherries.toString,
  //      if (value.inCone) "yes" else "no"
  //    )
  //}

  //implicit def pairEncoder[A, B](implicit ae: CsvEncoder[A], be: CsvEncoder[B]): CsvEncoder[(A, B)] = {
  //  pair: (A, B) => {
  //    val (a, b) = pair
  //    ae.encode(a) ++ be.encode(b)
  //  }
  //}

  val iceCreams: List[IceCream] = List(
    IceCream("A", 1, inCone = true),
    IceCream("B", 2, inCone = false),
    IceCream("X", 3, inCone = false)
  )

  implicit val booleanEncoder: CsvEncoder[Boolean] =
    if (_) List("yes") else List("no")

  implicit def numEncoder[A: Numeric]: CsvEncoder[A] =
    x => List(x.toString)

  implicit def doubleEnc: CsvEncoder[Double] =
    x => List(x.toString)

  implicit val stringEncoder: CsvEncoder[String] =
    List(_)

  implicit val hnilEncoder: CsvEncoder[HNil] =
    _ => List()

  implicit def hlistEncoder[H, T <: HList](implicit h: Lazy[CsvEncoder[H]], t: CsvEncoder[T]): CsvEncoder[H :: T] = {
    case x :: y => h.value.encode(x) ++ t.encode(y)
  }

  implicit def cnilEncoder: CsvEncoder[CNil] = {
    _ => {
      throw new Exception("wtf")
    }
  }


  implicit def coproductEncoder[H, T <: Coproduct](implicit h: Lazy[CsvEncoder[H]], t: CsvEncoder[T]): CsvEncoder[H :+: T] = {
    case Inl(head) => h.value.encode(head)
    case Inr(tail) => t.encode(tail)
  }

  implicit def genericEncoder[A, R](implicit generic: Generic[A] {type Repr = R}, enc: Lazy[CsvEncoder[R]]): CsvEncoder[A] =
    a => enc.value.encode(generic.to(a))

  val shapes: List[Shape]  = List(
    Rectangle(2.0, 3.0),
    Circle(1)
  )


}

object Run2 extends App {
  val employee: List[Employee] = List(
    Employee("Bill", 1, manager = true),
    Employee("Peter", 2, manager = false),
    Employee("Milton", 3, manager = false)
  )


  val trees : List[Tree[Int]] = List(
    Leaf(1),
    Branch(Leaf(5), Branch(Leaf(3), Leaf(4)))
  )

  println(writeCsv(employee))

  println(writeCsv(shapes))

  println(writeCsv(trees))
}



