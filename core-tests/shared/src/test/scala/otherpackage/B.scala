package otherpackage

import somepackage._
import zio._

trait B

val reproducer = ZLayer.makeSome[B, SomeLayer.A](SomeLayer.live)

