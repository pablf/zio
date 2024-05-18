package somepackage

object SomeLayer {

  case class A()

  val live: ZLayer[otherpackage.B, Nothing, A] = ???

}