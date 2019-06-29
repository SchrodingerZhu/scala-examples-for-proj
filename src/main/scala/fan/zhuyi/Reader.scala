package fan.zhuyi
import cats.instances.all._
import cats.syntax.all._
import cats.data.Reader

case class
Words(
      a: String,
      b: String,
      c: String,
      d: String
)

object ReaderTest {
  val aReader : Reader[Words, String] = Reader((x : Words) => x.a)
  val bReader : Reader[Words, String] = Reader((x : Words) => x.b)
  val cReader : Reader[Words, String] = Reader((x : Words) => x.c)
  val dReader : Reader[Words, String] = Reader((x : Words) => x.d)
  val allReader : Reader[Words, String] = for {
    a <- aReader
    b <- bReader
    c <- cReader
    d <- dReader
  } yield "a: %s, b: %s, c: %s, d: %s".format(a, b, c, d)
  val allReader2 : Reader[String, Words] = for {
    input <- Reader(identity[String])
    x     <- Reader((_ : String) => input + "123")
  } yield Words(input, input, input, x)
}
