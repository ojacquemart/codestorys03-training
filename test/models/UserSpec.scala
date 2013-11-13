package models

import org.specs2.mutable.Specification

import UserState._

object Ticker {
  def tick100times(user: User) = {
    for (i <- 1 to 100) user.tick(i)
  }
}

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

    "change toFloor state" in {
      val user = newUser
      user.isToFloorStateUndefined must beTrue
      user.setToFloorStateToNextToDefine
      user.isNextToFloorStateToNextToDefine must beTrue
      user.setToFloorStateToDefined
      user.toFloorState must be equalTo(ToFloorState.DEFINED)
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
      val user = new User(fromFloor = 5, toFloor = 7, UP)
      user.needsToGoUpFromFloorToFloor(7) must beFalse
      user.needsToGoUpFromFloorToFloor(6) must beFalse
      user.needsToGoUpFromFloorToFloor(5) must beFalse
      user.needsToGoUpFromFloorToFloor(4) must beTrue
    }

    "tick" in {
      val user = newUser
      user.tick(0)
      user.travelingTicks must be equalTo(0)
      user.waitingTicks must be equalTo(1)
      user.tick(1)
      user.travelingTicks must be equalTo(0)
      user.waitingTicks must be equalTo(2)

      user.travel()
      user.tick(0)
      user.travelingTicks must be equalTo(1)
      user.waitingTicks must be equalTo(2)
      user.tick(1)
      user.travelingTicks must be equalTo(2)
      user.waitingTicks must be equalTo(2)
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

  "Users" should {

    def threeUsers = {
      val users = new Users()
      users.add(0, UP)
      users.add(0, DOWN)
      users.add(10, DOWN)

      users
    }

    def newWaiterGoingUp(fromFloor: Int) = {
      new User(fromFloor, direction = UP)
    }

    "reset" in {
      val users = threeUsers

      users.size must be >=(0)
      users.reset
      users.size must be equalTo(0)
    }

    "add users and get size" in {
      val users = threeUsers

      users.size must be equalTo(3)
    }

    "flag next floor and go the toFloor" in {
      val users = new Users()
      users.add(0, UP) // go to 5
      users.add(2, DOWN) // go to 0
      users.add(2, UP) // go to 19

      // 0 to 5
      users.flagNextToFloorToDefine(0)
      users.goToFloor(5)
      users.users(0).toFloor must be equalTo(5)

      // 2 to 0
      users.flagNextToFloorToDefine(2)
      users.goToFloor(0)
      users.users(1).toFloor must be equalTo(0)

      // 2 to 19
      users.flagNextToFloorToDefine(2)
      users.goToFloor(19)
      users.users(2).toFloor must be equalTo(19)
    }


    /**
     * <li>users.add(10, 0, DOWN, TRAVELING)
     * <li>users.add(0, 2, UP, TRAVELING)
     * <li>users.add(1, 5, UP, TRAVELING)
     * <li>users.add(3, 5, UP, WAITING)
     * <li>users.add(4, 2, DOWN, WAITING)
     * <li>users.add(15, 13, DOWN, WAITING)
      */
    def usersWithTravelersAndWaiters = {
      val users = new Users()
      users.add(10, 0, DOWN, TRAVELING)
      users.add(0, 2, UP, TRAVELING)
      users.add(1, 5, UP, TRAVELING)
      users.add(3, 5, UP, WAITING)
      users.add(4, 2, DOWN, WAITING)
      users.add(15, 13, DOWN, WAITING)

      users
    }

    "stop when has travelers or waiters in a direction at a floor" in {
      val users = usersWithTravelersAndWaiters
      users.canStopAt(0, UP) must beTrue // traveler
      users.canStopAt(2, UP) must beTrue // traver
      users.canStopAt(5, UP) must beTrue // traver
      users.canStopAt(3, UP) must beTrue // waiter in same direction
      users.canStopAt(4, UP) must beFalse // waiter in inverse direction
      users.canStopAt(15, DOWN) must beTrue // waiter is in same direction
      users.canStopAt(19, UP) must beFalse // no one
    }

    /**
     * <li>users.add(2, 4, UP, WAITING)
     * <li>users.add(2, 0, DOWN, WAITING)
     * <li>users.add(15, 13, DOWN, WAITING)
     */
    def usersWithOnlyWaiters = {
      val users = new Users()
      users.add(2, 4, UP, WAITING)
      users.add(2, 0, DOWN, WAITING)
      users.add(15, 13, DOWN, WAITING)

      users
    }
    
    "stop when only waiters and no travelers" in {
      val users = usersWithOnlyWaiters
      users.canStopAt(2, UP) must beTrue
      users.canStopAt(2, DOWN) must beTrue
      users.canStopAt(15, UP) must beTrue
      users.canStopAt(19, DOWN) must beFalse // no one
    }

    def usersWithOnlyLosers = {
      val users = new Users
      users.add(0, 19, UP, TRAVELING)
      users.add(4, 8, UP, TRAVELING)
      users.add(10, 6, DOWN, TRAVELING)
      users.add(15, 11, DOWN, TRAVELING)
      
      users.users.foreach(u => tick100times(u))

      users
    }

    "stop when only travelers with score at 0" in {
      val users = usersWithOnlyLosers

      users.canStopAt(0, UP) must beFalse // no one
      users.canStopAt(17, UP) must beFalse // no one

      // All users does not give points
      users.users.forall(u => users.canStopAt(u.toFloor, UP) must beTrue)
    }

    def usersWithOnlyTravelersAtFloor10AndCabinSizeTo10 = {
      val users = new Users()
      users.maxTravelers = 10
      for (i <- 1 to users.maxTravelers) users.add(10, i, DOWN, TRAVELING)

      users
    }

    "don't stop max travelers is reached" in {
      val users = usersWithOnlyTravelersAtFloor10AndCabinSizeTo10
      users.travelersSize must be equalTo(10)
      users.hasMaxTravelers must beTrue

      users.add(10, UP)
      users.canStopAt(10, UP) must be equalTo(false)
    }

    "remove done users" in {
      val users = new Users()
      users.add(0, UP)
      users.add(0, DOWN)
      users.add(10, DOWN)
      users.checkStateAt(0)

      users.size must be equalTo(3)
      users.users.foreach(_.state = UserState.DONE)

      users.removeDone()
      users.size must be equalTo(0)
    }

    import NextDirectionType._

    "decide best direction when travelers" in {
      val users = usersWithTravelersAndWaiters
      users.getDirectionTypeForTravelers(1, UP) must be equalTo(SAME_AS_CURRENT)
      users.getDirectionTypeForTravelers(3, DOWN) must be equalTo(SAME_AS_CURRENT)
      users.getDirectionTypeForTravelers(5, DOWN) must be equalTo(SAME_AS_CURRENT)
      users.getDirectionTypeForTravelers(7, UP) must be equalTo(INVERSE)
    }

    "decide best direction when no travelers and only waiters" in {
      val users = usersWithOnlyWaiters

      /**
       *  "get calls in current direction when no go" in {
      elevator.resetToFloor(5)
      elevator.direction = DOWN
      strategy.reset

      strategy.getCallFromFloorFloorInCurrentDirection(elevator).size must be equalTo(0)
      strategy.addCall(fromFloor = 5, atFloor = 5, DOWN)
      strategy.getCallFromFloorFloorInCurrentDirection(elevator).size must be equalTo(1)
    }

        "find nearest call to go" in {
      strategy.reset
      strategy.addCall(fromFloor = 0, atFloor = 2, UP)
      strategy.addCall(fromFloor = 0, atFloor = 5, UP)
      strategy.addCall(fromFloor = 0, atFloor = 7, UP)
      strategy.addCall(fromFloor = 0, atFloor = 11, UP)
      strategy.addCall(fromFloor = 0, atFloor = 15, UP)
      strategy.addCall(fromFloor = 0, atFloor = 19, UP)

      strategy.nearestFloorByCurrentFloor(1) must be equalTo(2)
      strategy.nearestFloorByCurrentFloor(4) must be equalTo(5)
      strategy.nearestFloorByCurrentFloor(6) must be equalTo(5)
      strategy.nearestFloorByCurrentFloor(10) must be equalTo(11)
      strategy.nearestFloorByCurrentFloor(13) must be equalTo(11)
      strategy.nearestFloorByCurrentFloor(16) must be equalTo(15)
      strategy.nearestFloorByCurrentFloor(17) must be equalTo(15)
      strategy.nearestFloorByCurrentFloor(18) must be equalTo(19)
    }

       */
      // TODO: decide when no waiters
      1 == 1
    }

  }

}
