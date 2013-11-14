package models

import org.specs2.mutable.Specification

object ElevatorFuncSpec extends Specification {

  val MaxFloor = 19
  val MaxCabinSize = 100
  val elevator = new SimpleElevator(MaxFloor, MaxCabinSize, new OpenCloseStrategy())

  "Elevator Func" should {

    "get DOWN command when at top floor" in {
      elevator.floor = MaxFloor - 1
      elevator.direction = UP
      elevator.nextCommand() must be equalTo("DOWN")
    }

    "get UP command up when at bottom floor" in {
      elevator.floor = 0
      elevator.direction = DOWN
      elevator.nextCommand() must be equalTo("UP")
    }

    "not move if no one is in the cabin & at the middle floor" in {
      elevator.resetToFloor(10, MaxFloor, MaxCabinSize)
      elevator.nextCommand() must be equalTo("NOTHING")
      elevator.nextCommand() must be equalTo("NOTHING")
      elevator.nextCommand() must be equalTo("NOTHING")
      elevator.nextCommand() must be equalTo("NOTHING")
    }

    "not go and up or down continually with current in middle of two calls" in {
      elevator.resetToFloor(2, MaxFloor, MaxCabinSize)
      elevator.direction = UP
      elevator.call(atFloor = 0, UP)
      elevator.call(atFloor = 4, UP)
      elevator.nextCommand() must be equalTo("DOWN")
      elevator.nextCommand() must be equalTo("DOWN")
      elevator.nextCommand() must be equalTo("OPEN")
      elevator.nextCommand() must be equalTo("CLOSE")
      elevator.go(6)
      elevator.nextCommand() must be equalTo("UP")
      elevator.users.waiters.size must be equalTo(1)
      elevator.nextCommand() must be equalTo("UP")
    }

    "change direction when no go and one call" in {
      elevator.resetToFloor(5, MaxFloor, MaxCabinSize)
      elevator.direction = UP
      elevator.call(atFloor = 2, UP)
      elevator.nextCommand() must be equalTo("DOWN")
      elevator.nextCommand() must be equalTo("DOWN")
      elevator.nextCommand() must be equalTo("DOWN")
      elevator.nextCommand() must be equalTo("OPEN")
      elevator.nextCommand() must be equalTo("CLOSE")
      elevator.go(4)
      elevator.nextCommand() must be equalTo("UP")
    }

    "deserves a go and and has to search for calls in higher floors" in {
      elevator.resetToFloor(5, MaxFloor, MaxCabinSize)
      elevator.direction = UP
      elevator.callAndGo(atFloor = 5, toFloor = 3, DOWN)
      elevator.call(atFloor = 6, UP)
      elevator.nextCommand() must be equalTo("DOWN")
      elevator.nextCommand() must be equalTo("DOWN")
      elevator.nextCommand() must be equalTo("OPEN")
      elevator.users.travelers.size must be equalTo(0)
      elevator.nextCommand() must be equalTo("CLOSE")
      elevator.go(7)
      elevator.nextCommand() must be equalTo("UP")
    }

    "deserves a go and and has to search for calls in lower floors" in {
      elevator.resetToFloor(15, MaxFloor, MaxCabinSize)
      elevator.direction = UP
      elevator.reset
      elevator.callAndGo(atFloor = 15, toFloor = 17)
      elevator.call(atFloor = 6, UP)
      elevator.nextCommand() must be equalTo("UP")
      elevator.nextCommand() must be equalTo("UP")
      elevator.floor must be equalTo(17)
      elevator.nextCommand() must be equalTo("OPEN")
      elevator.nextCommand() must be equalTo("CLOSE")
      elevator.nextCommand() must be equalTo("DOWN")
    }

    "not keep two calls in different direction to the same floor outside" in {
      elevator.resetToFloor(10, MaxFloor, MaxCabinSize)
      elevator.call(atFloor = 10, UP)
      elevator.call(atFloor = 10, DOWN)

      elevator.nextCommand() must be equalTo("OPEN")
      elevator.go(12)
      elevator.go(0)
      elevator.nextCommand() must be equalTo("CLOSE")
      elevator.users.waiters.isEmpty must beTrue
    }

    "deserves two consecutive gos" in {
      elevator.resetToFloor(15, MaxFloor, MaxCabinSize)
      elevator.direction = UP
      elevator.callAndGo(atFloor = 15, toFloor = 17)
      elevator.callAndGo(atFloor = 15, toFloor = 18)
      elevator.nextCommand() must be equalTo("UP")
      elevator.nextCommand() must be equalTo("UP")
      elevator.floor must be equalTo(17)
      elevator.nextCommand() must be equalTo("OPEN")
      elevator.nextCommand() must be equalTo("CLOSE")
      elevator.nextCommand() must be equalTo("UP")
      elevator.floor must be equalTo(18)
      elevator.nextCommand() must be equalTo("OPEN")
      elevator.nextCommand() must be equalTo("CLOSE")
      elevator.nextCommand() must be equalTo("DOWN")
    }

    "does not need to stop if no go call or" in {
      elevator.resetToFloor(1, MaxFloor, MaxCabinSize)
      elevator.canStop() must beFalse
    }

    "force direction to middle when no another call or go in current direction" in {
      elevator.resetToFloor(5, MaxFloor, MaxCabinSize)
      elevator.call(atFloor = 4, UP)

      elevator.nextCommand() must be equalTo("DOWN")
      elevator.nextCommand() must be equalTo("OPEN")
      elevator.go(5)
      elevator.nextCommand() must be equalTo("CLOSE")
      elevator.nextCommand() must be equalTo("UP")
      elevator.nextCommand() must be equalTo("OPEN")
      elevator.nextCommand() must be equalTo("CLOSE")
      elevator.nextCommand() must be equalTo("UP")
      elevator.nextCommand() must be equalTo("UP")
      elevator.nextCommand() must be equalTo("UP")
      elevator.nextCommand() must be equalTo("UP")
      elevator.nextCommand() must be equalTo("UP")
      elevator.floor must be equalTo(elevator.middleFloor)
      elevator.users.waiters.isEmpty must beTrue
      elevator.users.travelers.isEmpty must beTrue
      elevator.nextCommand() must be equalTo("NOTHING")

    }

    "stops with a go at a floor" in {
      elevator.resetToFloor(1, MaxFloor, MaxCabinSize)
      elevator.direction = UP

      elevator.callAndGo(atFloor = 1, toFloor = 3)
      elevator.nextCommand() must be equalTo("UP")
      elevator.floor must be equalTo(2)
      elevator.nextCommand() must be equalTo("UP")
      elevator.floor must be equalTo(3)
      elevator.nextCommand() must be equalTo("OPEN")
      elevator.users.travelers.size must be equalTo(0)
      elevator.nextCommand() must be equalTo("CLOSE")
      elevator.nextCommand() must be equalTo("UP")
    }

    "stops with a a call at a floor in the same direction" in {
      elevator.resetToFloor(1, MaxFloor, MaxCabinSize)
      elevator.direction = UP

      elevator.call(atFloor = 3, UP)
      elevator.nextCommand() must be equalTo("UP")
      elevator.nextCommand() must be equalTo("UP")
      elevator.nextCommand() must be equalTo("OPEN")
      elevator.go(6)
      elevator.nextCommand() must be equalTo("CLOSE")
      elevator.users.waiters.size must be equalTo(0)
      elevator.nextCommand() must be equalTo("UP")
    }

    "stops with only one call at a floor in the opposite direction" in {
      elevator.resetToFloor(1, MaxFloor, MaxCabinSize)
      elevator.direction = UP

      elevator.call(atFloor = 3, DOWN)
      elevator.nextCommand() must be equalTo("UP")
      elevator.nextCommand() must be equalTo("UP")
      elevator.nextCommand() must be equalTo("OPEN")
      elevator.go(1)
      elevator.nextCommand() must be equalTo("CLOSE")
      elevator.users.waiters.size must be equalTo(0)
      elevator.nextCommand() must be equalTo("DOWN")
    }

    "stops with only one call at a floor in the opposite direction and then a go" in {
      elevator.resetToFloor(10, MaxFloor, MaxCabinSize)
      elevator.direction = DOWN

      elevator.call(atFloor = 8, UP)
      elevator.nextCommand() must be equalTo("DOWN")
      elevator.nextCommand() must be equalTo("DOWN")
      elevator.nextCommand() must be equalTo("OPEN")
      elevator.go(11)
      elevator.nextCommand() must be equalTo("CLOSE")
      elevator.users.waiters.size must be equalTo(0)
      elevator.nextCommand() must be equalTo("UP")
      elevator.nextCommand() must be equalTo("UP")
      elevator.nextCommand() must be equalTo("UP")
      elevator.nextCommand() must be equalTo("OPEN")
      elevator.nextCommand() must be equalTo("CLOSE")
      elevator.nextCommand() must be equalTo("DOWN")
    }

    "handle a go and a call in the same direction" in {
      elevator.resetToFloor(1, MaxFloor, MaxCabinSize)
      elevator.direction = UP

      elevator.callAndGo(atFloor = 1, toFloor = 5)
      elevator.call(atFloor = 3, UP)

      elevator.nextCommand() must be equalTo("UP")
      elevator.nextCommand() must be equalTo("UP")
      elevator.nextCommand() must be equalTo("OPEN")
      elevator.go(5)
      elevator.nextCommand() must be equalTo("CLOSE")
      elevator.users.waiters.size must be equalTo(0)
      elevator.nextCommand() must be equalTo("UP")
      elevator.nextCommand() must be equalTo("UP")
      elevator.nextCommand() must be equalTo("OPEN")
      elevator.users.travelers.size must be equalTo(0)
    }

    "handle a go and a call in the opposite direction" in {
      elevator.resetToFloor(1, MaxFloor, MaxCabinSize)
      elevator.direction = UP

      elevator.callAndGo(atFloor = 1, toFloor = 5)
      elevator.call(atFloor = 3, DOWN)

      elevator.nextCommand() must be equalTo("UP")
      elevator.floor must be equalTo(2)
      elevator.nextCommand() must be equalTo("UP")
      elevator.floor must be equalTo(3)
      elevator.nextCommand() must be equalTo("UP")
      elevator.floor must be equalTo(4)
      elevator.nextCommand() must be equalTo("UP")
      elevator.floor must be equalTo(5)
      elevator.nextCommand() must be equalTo("OPEN")
      elevator.users.travelers.size must be equalTo(0)
    }

    "not fucking go UP and DOWN" in {
      elevator.resetToFloor(5, MaxFloor, MaxCabinSize)
      elevator.direction = DOWN

      elevator.call(atFloor = 2, UP)
      elevator.call(atFloor = 7, DOWN)

      elevator.nextCommand() must be equalTo("UP")
      elevator.nextCommand() must be equalTo("UP")
      elevator.nextCommand() must be equalTo("OPEN")
      elevator.go(2)
      elevator.nextCommand() must be equalTo("CLOSE")
      elevator.nextCommand() must be equalTo("DOWN")
      elevator.nextCommand() must be equalTo("DOWN")
      elevator.nextCommand() must be equalTo("DOWN")
      elevator.nextCommand() must be equalTo("DOWN")
      elevator.nextCommand() must be equalTo("DOWN")
      elevator.nextCommand() must be equalTo("OPEN")
      elevator.go(5)
      elevator.nextCommand() must be equalTo("CLOSE")
      elevator.nextCommand() must be equalTo("UP")
    }

    /**
     * Travelers
     */
    def resetToFloor5WithCabinSizeAt10 ()= {
      val cabinSize = 10
      elevator.resetToFloor(5, MaxFloor, cabinSize)
    }

    "not take waiters if cabin is full with one waiter going in the same direction" in {
      resetToFloor5WithCabinSizeAt10()

      // 10 waiters going to 8 will enter on next command
      for (i <- 0 until 10) elevator.callAndGo(5, 8, UP)
      elevator.call(6, UP)

      elevator.nextCommand() must be equalTo("UP")
      elevator.users.travelersSize must be equalTo(10)
      elevator.users.waitersSize must be equalTo(1)
      elevator.nextCommand() must be equalTo("UP")
      elevator.nextCommand() must be equalTo("UP")
      elevator.nextCommand() must be equalTo("OPEN")
      elevator.nextCommand() must be equalTo("CLOSE")
      elevator.users.travelersSize must be equalTo(0)
      elevator.nextCommand() must be equalTo("DOWN")
      elevator.nextCommand() must be equalTo("DOWN")
      elevator.nextCommand() must be equalTo("OPEN")
      elevator.go(10)
      elevator.nextCommand() must be equalTo("CLOSE")
      elevator.nextCommand() must be equalTo("UP")
    }

    "not take waiters if cabin is full with one waiter going in the opposite direction" in {
      resetToFloor5WithCabinSizeAt10()

      // 10 waiters going to 8 will enter on next command
      for (i <- 0 until 10) elevator.callAndGo(5, 8, UP)
      elevator.call(6, DOWN)

      elevator.nextCommand() must be equalTo("UP")
      elevator.nextCommand() must be equalTo("UP")
      elevator.nextCommand() must be equalTo("UP")
      elevator.nextCommand() must be equalTo("OPEN")
      elevator.nextCommand() must be equalTo("CLOSE")
      elevator.nextCommand() must be equalTo("DOWN")
      elevator.nextCommand() must be equalTo("DOWN")
      elevator.nextCommand() must be equalTo("OPEN")
      elevator.go(7)
      elevator.nextCommand() must be equalTo("CLOSE")
      elevator.nextCommand() must be equalTo("UP")
      elevator.nextCommand() must be equalTo("OPEN")
      elevator.nextCommand() must be equalTo("CLOSE")
    }

    "let some travelers leave and waiters enter when playing with max cabin size" in {
      resetToFloor5WithCabinSizeAt10()

      // setup travelers at 6 to 10 and at 0 to 4
      for (i <- 0 until 5) elevator.callAndGo(5, 5 + (i+1), UP)
      for (i <- 0 until 5) elevator.callAndGo(5, 5 - (i+1), DOWN)
      // 4 people waiting at 6, will have to wait...
      elevator.call(6, UP)
      elevator.call(6, DOWN)

      elevator.nextCommand() must be equalTo("UP") // go to 6
      elevator.users.travelersSize must be equalTo(10)
      elevator.nextCommand() must be equalTo("OPEN") // one traveler leaves
      elevator.go(7) // one new enters
      elevator.nextCommand() must be equalTo("CLOSE")
      elevator.users.travelersSize must be equalTo(10)
      elevator.nextCommand() must be equalTo("UP") // go to 7
      elevator.users.travelersSize must be equalTo(10)

      elevator.nextCommand() must be equalTo("OPEN")
      elevator.users.travelersSize must be equalTo(8) // two travelers left
      elevator.nextCommand() must be equalTo("CLOSE")
      elevator.nextCommand() must be equalTo("UP") // go to 8
      elevator.nextCommand() must be equalTo("OPEN")
      elevator.users.travelersSize must be equalTo(7)
      elevator.nextCommand() must be equalTo("CLOSE")
      elevator.nextCommand() must be equalTo("UP") // go to 9
      elevator.nextCommand() must be equalTo("OPEN")
      elevator.users.travelersSize must be equalTo(6)
      elevator.nextCommand() must be equalTo("CLOSE")
      elevator.nextCommand() must be equalTo("UP") // go to 10
      elevator.nextCommand() must be equalTo("OPEN")
      elevator.users.travelersSize must be equalTo(5)
      elevator.nextCommand() must be equalTo("CLOSE")

      elevator.nextCommand() must be equalTo("DOWN") // floor 9
      elevator.nextCommand() must be equalTo("DOWN")
      elevator.nextCommand() must be equalTo("DOWN")
      elevator.nextCommand() must be equalTo("DOWN") // floor 6
      elevator.nextCommand() must be equalTo("OPEN")
      elevator.go(0)
      elevator.nextCommand() must be equalTo("CLOSE")
      elevator.nextCommand() must be equalTo("DOWN") // floor 5
      elevator.nextCommand() must be equalTo("DOWN") // floor 4
      elevator.nextCommand() must be equalTo("OPEN")
      elevator.nextCommand() must be equalTo("CLOSE")
      elevator.nextCommand() must be equalTo("DOWN") // floor 3
      elevator.nextCommand() must be equalTo("OPEN")
      elevator.nextCommand() must be equalTo("CLOSE")
      elevator.nextCommand() must be equalTo("DOWN") // floor 2
      elevator.nextCommand() must be equalTo("OPEN")
      elevator.nextCommand() must be equalTo("CLOSE")
      elevator.nextCommand() must be equalTo("DOWN") // floor 1
      elevator.nextCommand() must be equalTo("OPEN")
      elevator.nextCommand() must be equalTo("CLOSE")
      elevator.nextCommand() must be equalTo("DOWN") // floor 0
      elevator.nextCommand() must be equalTo("OPEN")
      elevator.nextCommand() must be equalTo("CLOSE")
      elevator.isEmpty must beTrue
      elevator.nextCommand() must be equalTo("UP") // go to middle
    }

  }


}
