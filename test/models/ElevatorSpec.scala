package models

import org.specs2.mutable.Specification

object ElevatorSpec extends Specification {

  val MaxFloor = 20
  val elevator = new SimpleElevator(MaxFloor, new OpenCloseStrategy())

  "Elevator" should {

    "get default values" in {
      elevator.floor should be equalTo (0)
      elevator.direction should be equalTo (UP)
      elevator.door should be equalTo(Door.CLOSE)
    }

    "reset values" in {
      elevator.direction = DOWN
      elevator.floor = 1
      elevator.door = Door.OPEN
      elevator.users.add(1, DOWN)

      elevator.resetToFloor(0)

      elevator.floor should be equalTo (0)
      elevator.users.size must be equalTo(0)
      elevator.direction should be equalTo (UP)
      elevator.door must be equalTo(Door.CLOSE)
    }

    "reset lower floor" in {
      elevator.floor = 2
      elevator.resetToFloor(10)
      elevator.floor must be equalTo(10)
    }

    "check if is at bottom floor" in {
      elevator.floor = 0
      elevator.isAtBottom must beTrue
      elevator.floor = 1
      elevator.isAtBottom must beFalse
    }

    "check if is at top floor" in {
      elevator.isAtTop must beFalse
      elevator.floor = MaxFloor - 1
      elevator.isAtTop must beTrue
    }

    "check if is at the middle floor" in {
      elevator.isAtMiddle must beFalse
      elevator.floor = 10
      elevator.isAtMiddle must beTrue
    }

    "check if needs to change current direction" in {
      elevator.floor = 1
      elevator.direction = UP
      elevator.needsToInverseDirection() must beFalse
      elevator.floor = MaxFloor - 1
      elevator.needsToInverseDirection() must beTrue
      elevator.direction = DOWN
      elevator.floor = 0
      elevator.needsToInverseDirection() must beTrue
    }

    "can do nothing when at middle floor" in {
      elevator.floor = 10
      elevator.canDoNothing() must beTrue
    }

  }

  "DirectionStrategy" should {

    val strategy = new DirectionStrategy()

    "get UP command from UP direction" in {
      strategy.fromDirection(UP) must be equalTo(UpCommand)
    }

    "get DOWN command from DOWN direction" in {
      strategy.fromDirection(DOWN) must be equalTo(DownCommand)
    }

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
      elevator.resetToFloor(10)
      elevator.nextCommand() must be equalTo("NOTHING")
      elevator.nextCommand() must be equalTo("NOTHING")
      elevator.nextCommand() must be equalTo("NOTHING")
      elevator.nextCommand() must be equalTo("NOTHING")
    }

    "force replacement at the middle floor" in {
      elevator.floor = 5
      strategy.forceDirectionToMiddleFloor(elevator) must be equalTo(UP)
      elevator.floor = 15
      strategy.forceDirectionToMiddleFloor(elevator) must be equalTo(DOWN)
    }

  }

  "OpenCloseStrategy" should {

    "not go and up or down continually with current in middle of two calls" in {
      elevator.resetToFloor(2)
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
      elevator.resetToFloor(5)
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
      elevator.resetToFloor(5)
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
      elevator.resetToFloor(15)
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
      elevator.resetToFloor(10)
      elevator.call(atFloor = 10, UP)
      elevator.call(atFloor = 10, DOWN)

      elevator.nextCommand() must be equalTo("OPEN")
      elevator.go(12)
      elevator.go(0)
      elevator.nextCommand() must be equalTo("CLOSE")
      elevator.users.waiters.isEmpty must beTrue
    }

    "deserves two consecutive gos" in {
      elevator.resetToFloor(15)
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
      elevator.resetToFloor(1)
      elevator.canStop() must beFalse
    }

    "force direction to middle when no another call or go in current direction" in {
      elevator.resetToFloor(5)
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
      elevator.nextCommand() must be equalTo("NOTHING")

    }

    "stops with a go at a floor" in {
      elevator.resetToFloor(1)
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
      elevator.resetToFloor(1)
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
      elevator.resetToFloor(1)
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
      elevator.resetToFloor(10)
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
      elevator.resetToFloor(1)
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
      elevator.resetToFloor(1)
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

    "not fucking fool me" in {
      elevator.resetToFloor(5)
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

  }





}
