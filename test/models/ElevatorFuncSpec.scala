package models

import org.specs2.mutable._
import org.specs2.specification._
import org.specs2.mutable.Specification
import play.api.Logger

object ElevatorFuncSpec extends Specification {

  val MaxFloor = 19
  val MaxCabinSize = 100

  class WithElevator(floor: Int = 0, cabinSize: Int = MaxCabinSize) extends Scope {
    Logger.debug("Elevator initialization")
    val elevator = new Elevator(0, MaxFloor, cabinSize, 1)
    firstCabin.floor = floor

    def firstCabin() = elevator.cabins(0)

    def nextCommand() = elevator.nextCommands().replace("\n", "")
  }

  "Elevator Func" should {

    "get DOWN command when at top floor" in new WithElevator(MaxFloor - 1) {
      firstCabin.direction = UP
      nextCommand must be equalTo("DOWN")
    }

    "get UP command up when at bottom floor" in new WithElevator {
      firstCabin.direction = DOWN
      nextCommand() must be equalTo("UP")
    }

    "not move if no one is in the cabin & at the middle floor" in new WithElevator(9) {
      nextCommand() must be equalTo("NOTHING")
      nextCommand() must be equalTo("NOTHING")
      nextCommand() must be equalTo("NOTHING")
      nextCommand() must be equalTo("NOTHING")
    }

    "not go and up or down continually with current in middle of two calls" in new WithElevator(2) {
      firstCabin.direction = UP
      elevator.call(0, UP)
      elevator.call(4, UP)
      nextCommand() must be equalTo("DOWN")
      nextCommand() must be equalTo("DOWN")
      nextCommand() must be equalTo("OPEN")
      elevator.userHasEntered(0)
      elevator.go(0, 6)
      nextCommand() must be equalTo("CLOSE")
      nextCommand() must be equalTo("UP")
      firstCabin.waiters.size must be equalTo(1)
      nextCommand() must be equalTo("UP")
    }

    "change direction when no go and one call" in new WithElevator(5) {
      firstCabin.direction = UP
      elevator.call(atFloor = 2, UP)
      nextCommand() must be equalTo("DOWN")
      nextCommand() must be equalTo("DOWN")
      nextCommand() must be equalTo("DOWN")
      nextCommand() must be equalTo("OPEN")
      nextCommand() must be equalTo("CLOSE")
      elevator.go(0, 4)
      nextCommand() must be equalTo("UP")
    }

    "deserves a go and and has to search for calls in higher floors" in new WithElevator(5) {
      firstCabin.direction = UP
      elevator.go(0, 3)
      elevator.call(6, UP)
      nextCommand() must be equalTo("DOWN")
      nextCommand() must be equalTo("DOWN")
      nextCommand() must be equalTo("OPEN")
      nextCommand() must be equalTo("CLOSE")
      nextCommand() must be equalTo("UP")
    }

    "deserves a go and and has to search for calls in lower floors" in new WithElevator(15) {
      firstCabin.direction = UP
      elevator.go(0, 17)
      elevator.call(atFloor = 6, UP)
      nextCommand() must be equalTo("UP")
      nextCommand() must be equalTo("UP")
      firstCabin.floor must be equalTo(17)
      nextCommand() must be equalTo("OPEN")
      nextCommand() must be equalTo("CLOSE")
      nextCommand() must be equalTo("DOWN")
    }

    "not keep two calls in different direction to the same floor outside" in new WithElevator(10) {
      elevator.call(atFloor = 10, UP)
      elevator.call(atFloor = 10, DOWN)

      nextCommand() must be equalTo("OPEN")
      elevator.userHasEntered(0)
      elevator.go(0, 12)
      elevator.userHasEntered(0)
      elevator.go(0, 0)
      nextCommand() must be equalTo("CLOSE")
      firstCabin.waiters.isEmpty must beTrue
    }

    "deserves two consecutive gos" in new WithElevator(15) {
      firstCabin.direction = UP
      elevator.go(0, 17)
      elevator.go(0, 18)
      nextCommand() must be equalTo("UP")
      nextCommand() must be equalTo("UP")
      firstCabin.floor must be equalTo(17)
      nextCommand() must be equalTo("OPEN")
      nextCommand() must be equalTo("CLOSE")
      nextCommand() must be equalTo("UP")
      firstCabin.floor must be equalTo(18)
      nextCommand() must be equalTo("OPEN")
      nextCommand() must be equalTo("CLOSE")
      nextCommand() must be equalTo("DOWN")
    }

    "force direction to middle when no another call or go in current direction" in new WithElevator(5) {
      elevator.call(atFloor = 4, UP)
      nextCommand() must be equalTo("DOWN")
      nextCommand() must be equalTo("OPEN")
      elevator.userHasEntered(0)
      elevator.go(0, 5)
      nextCommand() must be equalTo("CLOSE")
      nextCommand() must be equalTo("UP")
      nextCommand() must be equalTo("OPEN")
      nextCommand() must be equalTo("CLOSE")
      nextCommand() must be equalTo("UP")
      nextCommand() must be equalTo("UP")
      nextCommand() must be equalTo("UP")
      nextCommand() must be equalTo("UP")
      firstCabin.floor must be equalTo(firstCabin.middleFloor)
      firstCabin.waiters.isEmpty must beTrue
      firstCabin.travelers.isEmpty must beTrue
      nextCommand() must be equalTo("NOTHING")
    }

    "stops with a go at a floor" in new WithElevator(1) {
      firstCabin.direction = UP

      elevator.go(0, 3)
      nextCommand() must be equalTo("UP")
      nextCommand() must be equalTo("UP")
      nextCommand() must be equalTo("OPEN")
      nextCommand() must be equalTo("CLOSE")
      firstCabin.travelers.size must be equalTo(0)
      nextCommand() must be equalTo("UP")
    }

    "stops with a a call at a floor in the same direction" in new WithElevator(1) {
      firstCabin.direction = UP

      elevator.call(atFloor = 3, UP)
      nextCommand() must be equalTo("UP")
      nextCommand() must be equalTo("UP")
      nextCommand() must be equalTo("OPEN")
      elevator.userHasEntered(0)
      elevator.go(0, 6)
      nextCommand() must be equalTo("CLOSE")
      firstCabin.waiters.size must be equalTo(0)
      nextCommand() must be equalTo("UP")
    }

    "stops with only one call at a floor in the opposite direction" in new WithElevator(1) {
      firstCabin.direction = UP

      elevator.call(atFloor = 3, DOWN)
      nextCommand() must be equalTo("UP")
      nextCommand() must be equalTo("UP")
      nextCommand() must be equalTo("OPEN")
      elevator.userHasEntered(0)
      elevator.go(0, 1)
      nextCommand() must be equalTo("CLOSE")
      firstCabin.waiters.size must be equalTo(0)
      nextCommand() must be equalTo("DOWN")
    }

    "stops with only one call at a floor in the opposite direction and then a go" in new WithElevator(10) {
      firstCabin.direction = DOWN

      elevator.call(atFloor = 8, UP)
      nextCommand() must be equalTo("DOWN")
      nextCommand() must be equalTo("DOWN")
      nextCommand() must be equalTo("OPEN")
      elevator.userHasEntered(0)
      elevator.go(0, 11)
      nextCommand() must be equalTo("CLOSE")
      firstCabin.waiters.size must be equalTo(0)
      nextCommand() must be equalTo("UP")
      nextCommand() must be equalTo("UP")
      nextCommand() must be equalTo("UP")
      nextCommand() must be equalTo("OPEN")
      nextCommand() must be equalTo("CLOSE")
      nextCommand() must be equalTo("DOWN")
    }

    "handle a go and a call in the same direction"  in new WithElevator(1) {
      firstCabin.direction = UP

      elevator.go(0, 5)
      elevator.call(atFloor = 3, UP)

      nextCommand() must be equalTo("UP")
      nextCommand() must be equalTo("UP")
      nextCommand() must be equalTo("OPEN")
      elevator.userHasEntered(0)
      elevator.go(0, 5)
      nextCommand() must be equalTo("CLOSE")
      firstCabin.waiters.size must be equalTo(0)
      nextCommand() must be equalTo("UP")
      nextCommand() must be equalTo("UP")
      nextCommand() must be equalTo("OPEN")
      nextCommand() must be equalTo("CLOSE")
      firstCabin.travelers.size must be equalTo(0)
    }

    "handle a go and a call in the opposite direction"  in new WithElevator(1) {
      firstCabin.direction = UP

      elevator.go(0, 5)
      elevator.call(atFloor = 3, DOWN)

      nextCommand() must be equalTo("UP")
      nextCommand() must be equalTo("UP")
      nextCommand() must be equalTo("UP")
      nextCommand() must be equalTo("UP")
      nextCommand() must be equalTo("OPEN")
      nextCommand() must be equalTo("CLOSE")
      nextCommand() must be equalTo("DOWN")
      firstCabin.travelers.size must be equalTo(0)
    }

    "not fucking go UP and DOWN"  in new WithElevator(5) {
      firstCabin.direction = DOWN

      elevator.call(atFloor = 2, UP)
      elevator.call(atFloor = 7, DOWN)

      nextCommand() must be equalTo("UP")
      nextCommand() must be equalTo("UP")
      nextCommand() must be equalTo("OPEN")
      elevator.userHasEntered(0)
      elevator.go(0, 2)
      nextCommand() must be equalTo("CLOSE")
      nextCommand() must be equalTo("DOWN")
      nextCommand() must be equalTo("DOWN")
      nextCommand() must be equalTo("DOWN")
      nextCommand() must be equalTo("DOWN")
      nextCommand() must be equalTo("DOWN")
      nextCommand() must be equalTo("OPEN")
      elevator.userHasEntered(0)
      elevator.go(0, 5)
      nextCommand() must be equalTo("CLOSE")
      nextCommand() must be equalTo("UP")
    }

    "not take waiters if cabin is full with one waiter going in the same direction" in new WithElevator(5, cabinSize = 10) {

      // 10 waiters going to 8 will enter on next command
      for (i <- 0 until 10) elevator.go(0, 8)
      elevator.call(6, UP)

      nextCommand() must be equalTo("UP")
      firstCabin.travelers.size must be equalTo(10)
      firstCabin.waiters.size must be equalTo(1)
      nextCommand() must be equalTo("UP")
      nextCommand() must be equalTo("UP")
      nextCommand() must be equalTo("OPEN")
      nextCommand() must be equalTo("CLOSE")
      firstCabin.travelers.size must be equalTo(0)
      nextCommand() must be equalTo("DOWN")
      nextCommand() must be equalTo("DOWN")
      nextCommand() must be equalTo("OPEN")
      elevator.userHasEntered(0)
      elevator.go(0, 10)
      nextCommand() must be equalTo("CLOSE")
      nextCommand() must be equalTo("UP")
    }

    "not take waiters if cabin is full with one waiter going in the opposite direction"  in new WithElevator(5, cabinSize = 10) {
      // 10 waiters going to 8 will enter on next command
      for (i <- 0 until 10) elevator.go(0, 8)
      elevator.call(6, DOWN)

      nextCommand() must be equalTo("UP")
      nextCommand() must be equalTo("UP")
      nextCommand() must be equalTo("UP")
      nextCommand() must be equalTo("OPEN")
      nextCommand() must be equalTo("CLOSE")
      nextCommand() must be equalTo("DOWN")
      nextCommand() must be equalTo("DOWN")
      nextCommand() must be equalTo("OPEN")
      elevator.userHasEntered(0)
      elevator.go(0, 7)
      nextCommand() must be equalTo("CLOSE")
      nextCommand() must be equalTo("UP")
      nextCommand() must be equalTo("OPEN")
      nextCommand() must be equalTo("CLOSE")
    }

    "let some travelers leave and waiters enter when playing with max cabin size"  in new WithElevator(5, cabinSize = 10) {
      // setup travelers at 6 to 10 and at 0 to 4
      for (i <- 0 until 5) elevator.go(0, 5 + (i+1))
      for (i <- 0 until 5) elevator.go(0, 5 - (i+1))
      // 4 people waiting at 6, will have to wait...
      elevator.call(6, UP)
      elevator.call(6, DOWN)

      nextCommand() must be equalTo("UP") // go to 6
      firstCabin.travelers.size must be equalTo(10)
      nextCommand() must be equalTo("OPEN") // one traveler leaves
      elevator.userHasEntered(0)
      elevator.go(0, 7) // one new enters
      nextCommand() must be equalTo("CLOSE")
      firstCabin.travelers.size must be equalTo(10)
      nextCommand() must be equalTo("UP") // go to 7
      firstCabin.travelers.size must be equalTo(10)

      nextCommand() must be equalTo("OPEN")
      nextCommand() must be equalTo("CLOSE")
      firstCabin.travelers.size must be equalTo(8) // two travelers left

      nextCommand() must be equalTo("UP") // go to 8
      nextCommand() must be equalTo("OPEN")
      nextCommand() must be equalTo("CLOSE")
      firstCabin.travelers.size must be equalTo(7)
      nextCommand() must be equalTo("UP") // go to 9
      nextCommand() must be equalTo("OPEN")
      nextCommand() must be equalTo("CLOSE")
      firstCabin.travelers.size must be equalTo(6)
      nextCommand() must be equalTo("UP") // go to 10
      nextCommand() must be equalTo("OPEN")
      nextCommand() must be equalTo("CLOSE")
      firstCabin.travelers.size must be equalTo(5)

      nextCommand() must be equalTo("DOWN") // floor 9
      nextCommand() must be equalTo("DOWN")
      nextCommand() must be equalTo("DOWN")
      nextCommand() must be equalTo("DOWN") // floor 6
      nextCommand() must be equalTo("OPEN")
      elevator.userHasEntered(0)
      elevator.go(0, 0)
      nextCommand() must be equalTo("CLOSE")
      nextCommand() must be equalTo("DOWN") // floor 5
      nextCommand() must be equalTo("DOWN") // floor 4
      nextCommand() must be equalTo("OPEN")
      nextCommand() must be equalTo("CLOSE")
      nextCommand() must be equalTo("DOWN") // floor 3
      nextCommand() must be equalTo("OPEN")
      nextCommand() must be equalTo("CLOSE")
      nextCommand() must be equalTo("DOWN") // floor 2
      nextCommand() must be equalTo("OPEN")
      nextCommand() must be equalTo("CLOSE")
      nextCommand() must be equalTo("DOWN") // floor 1
      nextCommand() must be equalTo("OPEN")
      nextCommand() must be equalTo("CLOSE")
      nextCommand() must be equalTo("DOWN") // floor 0
      nextCommand() must be equalTo("OPEN")
      nextCommand() must be equalTo("CLOSE")
      firstCabin.isEmpty() must beTrue
      nextCommand() must be equalTo("UP") // go to middle
    }

  }
}
