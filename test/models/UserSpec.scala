package models

import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import play.api.Logger

object UserSpec extends Specification {

  import Ticker._

  "User" should {

    class WithUser(fromFloor: Int = 0, toFloor: Int = 5, direction: Direction = UP) extends Scope {
      val user = User(fromFloor, toFloor, cabin = 0, direction)
    }

    class WithWaiter extends Scope {
      val user = newUser
      user.state = UserState.WAITING
    }

    class WithTraveler(floor: Int = 0) extends Scope {
      val user = newUser
      user.travel()
      user.toFloor = floor
    }

    /**
     * <li>fromFloor = 0, toFloor = 5, direction = UP
     */
    def newUser = User(fromFloor = 0, toFloor = 5, direction = UP)

    "change state" in new WithUser {
      user.travel()
      user.state must be equalTo(UserState.TRAVELING)
      user.done()
      user.state must be equalTo(UserState.DONE)
      user.isDone should beTrue
    }

    "wait at a floor" in new WithWaiter {
      user.isWaiting() must beTrue
      user.isWaitingAt(2) must beFalse
      user.isWaitingAt(0) must beTrue
    }

    "wait at a floor in same direction" in new WithWaiter {
      user.isWaitingAtInDirection(0, UP) must beTrue
    }

    "travel at a floor" in {
      val user = newUser
      user.state = UserState.TRAVELING

      user.isTravelingAt(0) must beFalse
      user.isTravelingAt(5) must beTrue
    }

    "travel and check if gaining points" in new WithTraveler(5) {
      user.state = UserState.TRAVELING

      user.isTravelingAtAndScoring(0) must beFalse
      user.isTravelingAtAndScoring(5) must beTrue
    }

    "travel and tick a long time to not gaining points" in new WithTraveler(0) {
      for (i <- 1 to 100) user.tick(i)

      user.isTravelingAt(0) must beTrue
      user.isTravelingAtAndScoring(0) must beFalse
    }

    "travel at a floor" in new WithTraveler(5) {
      user.tick(0)
      user.isAt(0) must beTrue
      user.isAt(1) must beFalse
      user.tick(1)
      user.isAt(1) must beTrue
      user.isAt(0) must beFalse
    }

    "get direction by a current floor" in new WithUser {
      user.directionFromToFloorByFloor(0) must be equalTo(UP)
      user.directionFromToFloorByFloor(6) must be equalTo(DOWN)
    }

    "check if needs to up when waiting" in new WithUser(fromFloor = 5, toFloor = 7, direction = UP) {
      user.needsToGoUpFromFloorToFloor(7) must beFalse
      user.needsToGoUpFromFloorToFloor(6) must beFalse
      user.needsToGoUpFromFloorToFloor(5) must beFalse
      user.needsToGoUpFromFloorToFloor(4) must beTrue
    }

    "tick" in new WithUser {
      user.tick(0)
      user.travelingTime must be equalTo(0)
      user.waitingTime must be equalTo(1)
      user.tick(1)
      user.travelingTime must be equalTo(0)
      user.waitingTime must be equalTo(2)

      user.travel()
      user.tick(0)
      user.travelingTime must be equalTo(1)
      user.waitingTime must be equalTo(2)
      user.tick(1)
      user.travelingTime must be equalTo(2)
      user.waitingTime must be equalTo(2)
    }

    "score" in new WithUser {
      // waiting 10 ticks...
      for (i <- 0 until 10) user.tick(i)

      // traveling...
      user.travel()
      for (i <- 0 to 5) user.tick(i)

      user.score() must be equalTo(16)
    }

    "score 0 when long waiting and traveling times" in new WithUser {
      tick100times(user)

      user.travel()
      tick100times(user)

      user.score() must be equalTo(0)
    }

  }

}
