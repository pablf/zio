package somepackage

import zio._

object SomeLayer {

  case class A()

  val live: ZLayer[otherpackage.B, Nothing, A] = ZLayer { ZIO.service[otherpackage.B].map(_ => A()) }

}