package fan.zhuyi.Shapeless
import shapeless.Witness
import shapeless.labelled.{FieldType, KeyTag, field}
import shapeless.syntax.singleton._

object Tags extends App {
  val someNumber = 123
  val taggedNumber = "test" ->> someNumber

  def getField[K, V](value : FieldType[K, V]) (implicit  witness: Witness.Aux[K]) : K =
    witness.value

  println(getField(taggedNumber))

}
