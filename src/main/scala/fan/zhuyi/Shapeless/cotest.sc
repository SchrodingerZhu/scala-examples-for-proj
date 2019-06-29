import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr}
import cats.syntax.all._

sealed trait Shape
final case class Rectangle(width: Double, height: Double) extends Shape
final case class Circle(radius: Double) extends Shape


trait CsvEncoder[A] {
  def encode(value: A): List[String]
}

object CsvEncoder {
  def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] = enc

  def instance[A](func: A => List[String]): CsvEncoder[A] = {
    value: A => {
      func(value)
    }
  }


}

implicit class CsvWriter[T](value: T)(implicit enc: CsvEncoder[T]) {
  def toCSV: String = enc.encode(value).mkString(",")
}

case class Employee(name: String, number: Int, manager: Boolean)

case class IceCream(name: String, numCherries: Int, inCone: Boolean)

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
  IceCream("A", 1, true),
  IceCream("B", 2, false),
  IceCream("X", 3, false)
)



implicit val booleanEncoder: CsvEncoder[Boolean] =
  if (_) List("yes") else List("no")

implicit def numEncoder[A : Numeric] : CsvEncoder[A] =
  x => List(x.toString)

implicit val stringEncoder: CsvEncoder[String] =
  List(_)

implicit val hnilEncoder: CsvEncoder[HNil] =
  _ => List()

implicit def hlistEncoder[H, T <: HList](implicit h: CsvEncoder[H], t: CsvEncoder[T]): CsvEncoder[H :: T] =
  {
    case x :: y => h.encode(x) ++ t.encode(y)
  }

implicit def cnilEncoder: CsvEncoder[CNil] =
  throw new Exception("Bad day!")


implicit def coproductEncoder[H, T <: Coproduct](implicit h: CsvEncoder[H], t: CsvEncoder[T]): CsvEncoder[H :+: T] = {
  case Inl(x) => {
    h.encode(x)
  }
  case Inr(y) => {
    t.encode(y)
  }
}


implicit def genericEncoder[A, R] (
  implicit
  generic: Generic[A] {type Repr = R},
  enc: CsvEncoder[R]
) : CsvEncoder[A] = a => enc.encode(generic.to(a))

implicit val e = Generic[Employee]

implicit val i = Generic[IceCream]

implicit val s = Generic[Shape]

implicit val o = Generic[Rectangle]

implicit val p = Generic[Circle]




val shapes : List[Shape] = List(
  Rectangle(3.0, 0.0),
  Circle(1.0)
)
writeCsv(employee)

writeCsv(employee zip iceCreams)

writeCsv(shapes)
