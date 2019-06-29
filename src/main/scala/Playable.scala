trait Playable[A] {
  def runPlay(x:A) : String
}

object PlayableInstances {
  implicit object PlayIntInstances extends Playable[Int] {
    def runPlay(x:Int) : String = "Int: " + x.toString
  }

  implicit object PlayStringInstances extends Playable[String] {
    def runPlay(x:String) : String = "Str: " + x.toString
  }

  implicit def playListInstances[A](implicit playable: Playable[A]) = new Playable[List[A]] {
    def runPlay(x : List[A]) : String = x.map(playable.runPlay).toString()
  }
}

object PlayUtil {
  def playfy[A](x : A)(implicit playable: Playable[A]) = {
    playable.runPlay(x)
  }

  implicit class PlaySyntax[T](data: T) {
    def toPlay(implicit playable: Playable[T]) = playable.runPlay(data)
  }
}

