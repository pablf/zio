import zio.test._

import java.util.Properties
import scala.language.unsafeNulls

object NNBugSpec extends ZIOSpecDefault {
  override def spec: Spec[Any, Throwable] =
    suite("NNBugSpec")(
      test("will work just fine, because a true Assertion on a .nn Succeeds") {
        val someProperties = new Properties()
        someProperties.setProperty("someKey", "someValue")
        assertTrue(someProperties.getProperty("someKey").nn == "someValue")
      },
      test("will fail just fine, because a false Assertion Fails") {
        val someProperties = new Properties()
        someProperties.setProperty("someKey", "someValue")
        assertTrue(someProperties.getProperty("someKey") == "notSomeValue")
      },
      test("will explode with java.lang.StringIndexOutOfBoundsException because a false Assertion on a .nn fails to parse properly") {
        val someProperties = new Properties()
        someProperties.setProperty("someKey", "someValue")
        assertTrue(someProperties.getProperty("someKey").nn == "notSomeValue")
      }
    )
}