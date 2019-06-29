package fan.zhuyi.Shapeless
import shapeless.Generic
case class IceCream (name: String, numCherries: Int, inCone: Boolean)
case class Employee(name: String, number: Int, manager: Boolean)


object IceCream extends App {
  val iceCreamGen = Generic[IceCream]
  val iceCream = IceCream("Sundae", 1, inCone = false)
  val repr = iceCreamGen.to(iceCream)
  val iceCream2 = iceCreamGen.from(repr)
  val employee = Generic[Employee].from(repr)
  println(employee)
}


