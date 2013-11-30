package models

import org.specs2.mutable.Specification

object UserSpec extends Specification {

  import Ticker._

  "User" should {

    /**
     * <li>fromFloor = 0, toFloor = 5, direction = UP
     */
    def newUser = User(fromFloor = 0, toFloor = 5, direction = UP)

    "change state" in {
      val user = newUser
      user.travel()
      user.state must be equalTo(UserState.TRAVELING)
      user.done()
      user.state must be equalTo(UserState.DONE)
      user.isDone should beTrue
    }

    def newWaiter = {
      val user = newUser
      user.state = UserState.WAITING
      user
    }

    "wait at a floor" in {
      val user = newWaiter
      user.isWaiting() must beTrue
      user.isWaitingAt(2) must beFalse
      user.isWaitingAt(0) must beTrue
    }

    "wait at a floor in same direction" in {
      val user = newWaiter
      user.isWaitingAtInDirection(0, UP) must beTrue
    }

    def newTraveler(floor: Int = 0) = {
      val user = newUser
      user.travel()
      user.toFloor = floor
      user
    }

    "travel at a floor" in {
      val user = newUser
      user.state = UserState.TRAVELING

      user.isTravelingAt(0) must beFalse
      user.isTravelingAt(5) must beTrue
    }

    "travel at a floor gaining points!" in {
      val user = newTraveler(5)
      user.state = UserState.TRAVELING

      user.isTravelingAtAndScoring(0) must beFalse
      user.isTravelingAtAndScoring(5) must beTrue

      val lostUser = newTraveler(0)
      for (i <- 1 to 100) lostUser.tick(i)

      lostUser.isTravelingAt(0) must beTrue
      lostUser.isTravelingAtAndScoring(0) must beFalse
    }

    "travel at a floor" in {
      val user = newTraveler(5)
      user.tick(0)
      user.isAt(0) must beTrue
      user.isAt(1) must beFalse
      user.tick(1)
      user.isAt(1) must beTrue
      user.isAt(0) must beFalse
    }

    "get direction by a current floor" in {
      val user = newUser // toFloor is 5
      user.directionFromToFloorByFloor(0) must be equalTo(UP)
      user.directionFromToFloorByFloor(6) must be equalTo(DOWN)
    }

    "check if needs to up when waiting" in {
      val user = new User(fromFloor = 5, toFloor = 7, cabin = 0, direction = UP)
      user.needsToGoUpFromFloorToFloor(7) must beFalse
      user.needsToGoUpFromFloorToFloor(6) must beFalse
      user.needsToGoUpFromFloorToFloor(5) must beFalse
      user.needsToGoUpFromFloorToFloor(4) must beTrue
    }

    "tick" in {
      val user = newUser
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

    "score" in {
      val user = newUser
      // waiting 10 ticks...
      for (i <- 0 until 10) user.tick(i)

      // traveling...
      user.travel()
      for (i <- 0 to 5) user.tick(i)

      user.score() must be equalTo(16)
    }

    "score 0 when long waiting and traveling times" in {
      val user = newUser
      tick100times(user)

      user.travel()
      tick100times(user)

      user.score() must be equalTo(0)
    }

  }

}
