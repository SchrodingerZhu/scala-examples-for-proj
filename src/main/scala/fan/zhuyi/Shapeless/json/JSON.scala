package fan.zhuyi.Shapeless.json
import fan.zhuyi.Shapeless.{Circle, IceCream, Rectangle, Shape}
import shapeless.labelled.FieldType
import JsonEncoder._
import shapeless.{:+:, ::, CNil, Coproduct, HList, HNil, Inl, Inr, LabelledGeneric, Lazy, Witness}
sealed trait JsonValue {
  def write : String = this match {
    case JsonNull => ""
    case JsonBoolean(v) => v.toString
    case JsonNumber(v) => v.toString
    case JsonString(v) => "\"%s\"".format(v)
    case JsonArray(items) => "[" + items.map(x => x.write).mkString(", ") + "]"
    case JsonObject(fields) => "{" + fields.map(x => "\"%s\": %s".format(x._1, x._2.write)).mkString(", ") + "}"
  }
}

case class JsonObject(fields: List[(String, JsonValue)]) extends JsonValue
case class JsonArray(items: List[JsonValue]) extends  JsonValue
case class JsonString(value: String) extends JsonValue
case class JsonNumber[A : Numeric](value: A) extends  JsonValue
case class JsonBoolean(value: Boolean) extends JsonValue
case object JsonNull extends JsonValue

trait JsonEncoder[A] {
  def encode(value: A): JsonValue
}

trait JsonObjectEncoder[A] extends JsonEncoder[A] {
  override def encode(value: A): JsonObject
}

object JsonEncoder {
  def apply[A](implicit enc: JsonEncoder[A]): JsonEncoder[A] = enc
  def create[A](func: A => JsonValue): JsonEncoder[A] = func(_)
  implicit val stringEncoder: JsonEncoder[String] =
    JsonString(_)
  implicit def numericEncoder[A : Numeric] : JsonEncoder[A] =
    JsonNumber[A](_)
  implicit def listEncoder[A](implicit encoder: JsonEncoder[A]) : JsonEncoder[List[A]] =
    list => JsonArray(list.map(encoder.encode))
  implicit val booleanEncoder : JsonEncoder[Boolean] =
    JsonBoolean(_)
  implicit def optionEncoder[A](implicit encoder: JsonEncoder[A]): JsonEncoder[Option[A]] =
    opt => opt.map(encoder.encode).getOrElse(JsonNull)
  implicit class classSyntax[A](value: A) {
    def toJSON(implicit encoder: JsonEncoder[A]) : JsonValue = encoder.encode(value)
  }

  implicit val hnilEncoder: JsonObjectEncoder[HNil] =
    _ => JsonObject(Nil)
  implicit def hlistObjectEncoder[K <: Symbol, H, T <: HList](implicit witness: Witness.Aux[K], h: Lazy[JsonEncoder[H]], t: JsonObjectEncoder[T]) :
  JsonObjectEncoder[FieldType[K, H] :: T] = {
    val fieldName : String = witness.value.name
    hlist => {
      val head = h.value.encode(hlist.head)
      val tail = t.encode(hlist.tail)
      JsonObject((fieldName, head) :: tail.fields)
    }
  }

  implicit val cnilObjectEncoder: JsonObjectEncoder[CNil] =
    _ => throw new Exception("BAD JSON Converting!")

  implicit def coproductObjectEncoder [K <: Symbol, H, T <: Coproduct] (implicit witness: Witness.Aux[K], h: Lazy[JsonEncoder[H]], t: JsonObjectEncoder[T]) :
  JsonObjectEncoder[FieldType[K, H] :+: T] = {
    val fieldName : String = witness.value.name

    (value : FieldType[K, H] :+: T) => value match {
      case Inl(head) => JsonObject(List(fieldName -> h.value.encode(head)))
      case Inr(tail) => t.encode(tail)
    }
  }

  implicit def genericObjectEncoder[A , H](implicit generic: LabelledGeneric.Aux[A, H], encoder: Lazy[JsonObjectEncoder[H]]) : JsonEncoder[A] =
    value => encoder.value.encode(generic.to(value))


}

case class Test(name : String, iceCreams: List[IceCream], shapes: List[Shape])

object RunJson extends App {
  val iceCreams  = List(
    IceCream("A", 1, inCone = false),
    IceCream("B", 2, inCone = false),
  )
  val shapes : List[Shape] = List (
    Circle(1),
    Rectangle(4, 5)
  )
  val test = Test("miku", iceCreams, shapes)
  print(test.toJSON.write)
}
