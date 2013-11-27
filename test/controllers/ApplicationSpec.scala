package controllers

import scala.concurrent.Future

import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

import play.api.mvc.SimpleResult

import play.api.test._
import play.api.test.Helpers._

/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 * For more information, consult the wiki.
 */
@RunWith(classOf[JUnitRunner])
class ApplicationSpec extends Specification {

  "Application" should {

    def assert200(routeToTest: String): Future[SimpleResult] = {
      val ok = route(FakeRequest(GET, routeToTest)).get
      status(ok) must be equalTo (OK)

      ok
    }

    def assertNextsCommands(cabinCount: Int) = {
      val result = assert200("/nextCommands")
      val commands = contentAsString(result).split("\n")
      commands.size must be equalTo(cabinCount)
    }

    "send 200 on reset with one cabin" in new WithApplication {
      assert200("/reset?lowerFloor=0&higherFloor=19&cabinSize=30&cabinCount=1&cause=Bad+request")
    }
    "send 200 on nextCommands" in new WithApplication {
      assertNextsCommands(1)
    }
    "send 200 on reset with two cabins" in new WithApplication {
      assert200("/reset?lowerFloor=0&higherFloor=19&cabinSize=30&cabinCount=2&cause=Bad+request")
    }
    "send 200 on nextCommands" in new WithApplication {
      assertNextsCommands(2)
    }
    "send 200 on call" in new WithApplication {
      assert200("/call?atFloor=1&to=UP")
    }
    "send 200 on go" in new WithApplication {
      assert200("/go?cabin=0&floorToGo=2")
    }

    "send 200 on userHasEntered" in new WithApplication {
      assert200("/userHasEntered?cabin=0")
    }

    "send 200 on userHasExited" in new WithApplication {
      assert200("/userHasExited?cabin=1")
    }
  }
}
