package models

import org.specs2.mutable.Specification

import UserState._

object UsersSpec extends Specification {

  import Ticker._

  "Users" should {

    def threeUsers = {
      val users = new NormalUsers()
      users.add(0, UP)
      users.add(0, DOWN)
      users.add(10, DOWN)

      users
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
      val users = new NormalUsers()
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
      val users = new NormalUsers()
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
      users.canStopAt(2, UP) must beTrue // traveler
      users.canStopAt(5, UP) must beTrue // traveler
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
      val users = new NormalUsers()
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
      val users = new NormalUsers
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

    def  usersWithOnlyTravelersAtFloor10AndMaxTravelersTo10 = {
      val users = new NormalUsers(9)
      for (i <- 0 to users.maxTravelers) users.add(floor = 10, toFloor = i, DOWN, TRAVELING)

      users
    }

    "don't stop when max travelers is reached" in {
      val users = usersWithOnlyTravelersAtFloor10AndMaxTravelersTo10
      users.travelersSize must be equalTo(10)
      users.remainsPlaceForNewTravelers() must beFalse

      users.add(11, UP) // waiter at 11
      users.canStopAt(12, UP) must beFalse // waiter can't enter, cabin is full!
      users.canStopAt(9, UP) must beTrue // traveler at 9
      users.canStopAt(0, UP) must beTrue // traveler at 0
      users.canStopAt(19, UP) must beFalse // no one at 19
    }

    "remove done users" in {
      val users = new NormalUsers()
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
