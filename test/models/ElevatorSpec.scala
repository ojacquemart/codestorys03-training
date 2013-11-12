package models

import org.specs2.mutable.Specification

object ElevatorSpec extends Specification {

  val MaxFloor = 20
  val elevator = new SimpleElevator(MaxFloor, new OpenCloseStrategy())

  val emptyUser = new User(1, DOWN)

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
      elevator.users +=

      elevator.resetToFloor(0)

      elevator.floor should be equalTo (0)
      elevator.users must be equalTo(0)
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

    "count users" in {
      elevator.users must be equalTo(0)
      elevator.userHasEntered
      elevator.users must be equalTo(1)
      elevator.onUserExited
      elevator.users must be equalTo(0)
    }

    "reset users count" in {
      elevator.resetToFloor(0)
      elevator.userHasEntered
      elevator.users must be equalTo(1)

      elevator.resetToFloor(0)
      elevator.users must be equalTo(0)
    }

  }

  "Waiters" should {

    val waiters = new Waiters()

    "count waiters by floor" in {
      waiters.addAt(1)
      waiters.addAt(1)
      waiters.addAt(3)

      waiters.countAt(0) must be equalTo(0)
      waiters.countAt(1) must be equalTo(2)
      waiters.countAt(2) must be equalTo(0)
      waiters.countAt(3) must be equalTo(1)

      // Enters at floor 0
      waiters.removeAt(1)
      waiters.removeAt(1)
      waiters.countAt(1) must be equalTo(0)

      waiters.removeAt(3)
      waiters.countAt(3) must be equalTo(0)

      waiters.addAt(0)
      waiters.addAt(1)
      waiters.reset
      waiters.filterByPositive().size must be equalTo(0)
    }

  }

  "DirectionStrategy" should {

    val strategy = new DirectionStrategy()

    "reset stops" in {
      strategy.addGo(fromFloor = 0, toFloor = 1)
      strategy.addCall(fromFloor = 0, atFloor = 1, UP)
      strategy.gos.size must be equalTo(1)
      strategy.calls.size must be equalTo(1)

      strategy.reset
      strategy.gos.size must be equalTo(0)
      strategy.calls.size must be equalTo(0)
    }

    "can do nothing" in {
      elevator.floor = 10

      strategy.reset
      strategy.canDoNothing(elevator) must beTrue
    }

    "get UP command from UP direction" in {
      strategy.fromDirection(UP) must be equalTo(UpCommand)
    }

    "get DOWN command from DOWN direction" in {
      strategy.fromDirection(DOWN) must be equalTo(DownCommand)
    }

    "get DOWN command when at top floor" in {
      elevator.floor = MaxFloor - 1
      elevator.direction = UP
      strategy.nextCommand(elevator) must be equalTo("DOWN")
    }

    "get UP command up when at bottom floor" in {
      elevator.floor = 0
      elevator.direction = DOWN
      strategy.nextCommand(elevator) must be equalTo("UP")
    }

    "not move if no one is in the cabin & at the middle floor" in {
      elevator.resetToFloor(10)
      strategy.reset
      strategy.nextCommand(elevator) must be equalTo("NOTHING")
      strategy.nextCommand(elevator) must be equalTo("NOTHING")
      strategy.nextCommand(elevator) must be equalTo("NOTHING")
      strategy.nextCommand(elevator) must be equalTo("NOTHING")
    }

    "force replacement at the middle floor" in {
      elevator.floor = 5
      strategy.reset

      strategy.forceDirectionToMiddleFloor(elevator) must be equalTo(UP)

      elevator.floor = 15
      strategy.forceDirectionToMiddleFloor(elevator) must be equalTo(DOWN)
    }

    "find best direction by current direction from up to up" in {
      elevator.floor = 5
      elevator.direction = UP
      strategy.reset
      strategy.addGo(fromFloor = 5, toFloor = 12)
      strategy.findBestDirectionByCurrentDirection(elevator) must be equalTo(UP)
    }

    "find best direction by current direction from up to down" in {
      elevator.floor = 15
      elevator.direction = UP
      strategy.reset
      strategy.addGo(fromFloor = 15, toFloor = 12)
      strategy.findBestDirectionByCurrentDirection(elevator) must be equalTo(DOWN)
    }

  }

  "OpenCloseStrategy" should {
    
    val strategy = new OpenCloseStrategy()

    "gets stops from floor" in {
      strategy.addGo(fromFloor = 0, toFloor = 10)
      strategy.getStopFromFloor(10).size must be equalTo(1)
    }

    "get calls in current direction when no go" in {
      elevator.resetToFloor(5)
      elevator.direction = DOWN
      strategy.reset

      strategy.getCallFromFloorFloorInCurrentDirection(elevator).size must be equalTo(0)
      strategy.addCall(fromFloor = 5, atFloor = 5, DOWN)
      strategy.getCallFromFloorFloorInCurrentDirection(elevator).size must be equalTo(1)
    }

    "not go and up or down continually with current in middle of two calls" in {
      elevator.resetToFloor(2)
      strategy.reset
      strategy.addCall(fromFloor = 2, atFloor = 0, UP)
      strategy.addCall(fromFloor = 2, atFloor = 4, UP)
      strategy.nextCommand(elevator) must be equalTo("DOWN")
      strategy.nextCommand(elevator) must be equalTo("DOWN")
      strategy.nextCommand(elevator) must be equalTo("OPEN")
      strategy.nextCommand(elevator) must be equalTo("CLOSE")
      strategy.nextCommand(elevator) must be equalTo("UP")
      strategy.nextCommand(elevator) must be equalTo("UP")
    }

    "change direction is no go and one call" in {
      elevator.resetToFloor(5)
      elevator.direction = UP
      strategy.reset
      strategy.addCall(fromFloor = 5, atFloor = 2, DOWN)
      strategy.nextCommand(elevator) must be equalTo("DOWN")
      strategy.nextCommand(elevator) must be equalTo("DOWN")
      strategy.nextCommand(elevator) must be equalTo("DOWN")
      strategy.nextCommand(elevator) must be equalTo("OPEN")
      strategy.nextCommand(elevator) must be equalTo("CLOSE")
      strategy.nextCommand(elevator) must be equalTo("UP")
    }

    "deserves a go and and has to search for calls in higher floors" in {
      elevator.resetToFloor(5)
      elevator.direction = UP
      strategy.reset
      strategy.addGo(fromFloor = 5, toFloor = 3)
      strategy.addCall(fromFloor = 5, atFloor = 6, UP)
      strategy.nextCommand(elevator) must be equalTo("DOWN")
      strategy.nextCommand(elevator) must be equalTo("DOWN")
      strategy.nextCommand(elevator) must be equalTo("OPEN")
      strategy.nextCommand(elevator) must be equalTo("CLOSE")
      strategy.nextCommand(elevator) must be equalTo("UP")
    }

    "deserves a go and and has to search for calls in lower floors" in {
      elevator.resetToFloor(15)
      elevator.direction = UP
      strategy.reset
      strategy.addGo(fromFloor = 15, toFloor = 17)
      strategy.addCall(fromFloor = 15, atFloor = 6, UP)
      strategy.nextCommand(elevator) must be equalTo("UP")
      strategy.nextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(17)
      strategy.nextCommand(elevator) must be equalTo("OPEN")
      strategy.nextCommand(elevator) must be equalTo("CLOSE")
      strategy.nextCommand(elevator) must be equalTo("DOWN")
    }

    "not keep two calls in different direction to the same floor in the system" in {
      elevator.resetToFloor(10)
      strategy.reset
      strategy.addCall(fromFloor = 10, atFloor = 10, UP)
      strategy.addCall(fromFloor = 10, atFloor = 10, DOWN)

      strategy.nextCommand(elevator) must be equalTo("OPEN")
      strategy.calls.size must be equalTo(0)
    }


    "deserves two consecutive gos" in {
      elevator.resetToFloor(15)
      elevator.direction = UP
      strategy.reset
      strategy.addGo(fromFloor = 15, toFloor = 17)
      strategy.addGo(fromFloor = 15, toFloor = 18)
      strategy.nextCommand(elevator) must be equalTo("UP")
      strategy.nextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(17)
      strategy.nextCommand(elevator) must be equalTo("OPEN")
      strategy.nextCommand(elevator) must be equalTo("CLOSE")
      strategy.nextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(18)
      strategy.nextCommand(elevator) must be equalTo("OPEN")
      strategy.nextCommand(elevator) must be equalTo("CLOSE")
      strategy.nextCommand(elevator) must be equalTo("DOWN")
    }

    "does not need to stop if no go call or" in {
      elevator.resetToFloor(1)
      strategy.reset
      strategy.needsStop(elevator) must beFalse
    }

    "force direction to middle when no another call or go in current direction" in {
      elevator.resetToFloor(5)

      strategy.reset
      strategy.addCall(fromFloor = 5, atFloor = 4, UP)

      strategy.nextCommand(elevator) must be equalTo("DOWN")
      strategy.nextCommand(elevator) must be equalTo("OPEN")
      strategy.nextCommand(elevator) must be equalTo("CLOSE")
      strategy.nextCommand(elevator) must be equalTo("UP")
      strategy.nextCommand(elevator) must be equalTo("UP")
      strategy.nextCommand(elevator) must be equalTo("UP")
      strategy.nextCommand(elevator) must be equalTo("UP")
      strategy.nextCommand(elevator) must be equalTo("UP")
      strategy.nextCommand(elevator) must be equalTo("UP")
      strategy.nextCommand(elevator) must be equalTo("NOTHING")

    }

    "stops with a go at a floor" in {
      elevator.resetToFloor(1)
      elevator.direction = UP
      strategy.reset

      strategy.addGo(fromFloor = 1, toFloor = 3)
      strategy.nextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(2)
      strategy.nextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(3)
      strategy.nextCommand(elevator) must be equalTo("OPEN")
      strategy.gos.size must be equalTo(0)
      strategy.nextCommand(elevator) must be equalTo("CLOSE")
      strategy.nextCommand(elevator) must be equalTo("UP")
    }

    "stops with a a call at a floor in the same direction" in {
      elevator.resetToFloor(1)
      elevator.direction = UP
      strategy.reset

      strategy.addCall(fromFloor = 1, atFloor = 3, UP)
      strategy.nextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(2)
      strategy.nextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(3)
      strategy.nextCommand(elevator) must be equalTo("OPEN")
      strategy.calls.size must be equalTo(0)
      strategy.nextCommand(elevator) must be equalTo("CLOSE")
      strategy.nextCommand(elevator) must be equalTo("UP")
    }

    "stops with only one call at a floor in the opposite direction" in {
      elevator.resetToFloor(1)
      elevator.direction = UP
      strategy.reset

      strategy.addCall(fromFloor = 1, atFloor = 3, DOWN)
      strategy.nextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(2)
      strategy.nextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(3)
      strategy.nextCommand(elevator) must be equalTo("OPEN")
      strategy.calls.size must be equalTo(0)
      strategy.nextCommand(elevator) must be equalTo("CLOSE")
      strategy.nextCommand(elevator) must be equalTo("UP")
    }

    "stops with only one call at a floor in the opposite direction and then a go" in {
      elevator.resetToFloor(1)
      elevator.direction = UP
      strategy.reset

      strategy.addCall(fromFloor = 1, atFloor = 3, DOWN)
      strategy.nextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(2)
      strategy.nextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(3)
      strategy.nextCommand(elevator) must be equalTo("OPEN")
      strategy.calls.size must be equalTo(0)
      strategy.nextCommand(elevator) must be equalTo("CLOSE")
      strategy.addGo(elevator.floor, 2)
      strategy.nextCommand(elevator) must be equalTo("DOWN")
      strategy.nextCommand(elevator) must be equalTo("OPEN")
      strategy.nextCommand(elevator) must be equalTo("CLOSE")
      strategy.nextCommand(elevator) must be equalTo("UP")
    }

    "handle a go and a call in the same direction" in {
      elevator.resetToFloor(1)
      elevator.direction = UP
      strategy.reset

      strategy.addGo(fromFloor = 1, toFloor = 5)
      strategy.addCall(fromFloor = 1, atFloor = 3, UP)

      strategy.nextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(2)
      strategy.nextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(3)
      strategy.nextCommand(elevator) must be equalTo("OPEN")
      strategy.calls.size must be equalTo(0)
      strategy.nextCommand(elevator) must be equalTo("CLOSE")
      strategy.nextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(4)
      strategy.nextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(5)
      strategy.nextCommand(elevator) must be equalTo("OPEN")
      strategy.gos.size must be equalTo(0)
    }

    "handle a go and a call in the opposite direction" in {
      elevator.resetToFloor(1)
      elevator.direction = UP
      strategy.reset

      strategy.addGo(fromFloor = 1, toFloor = 5)
      strategy.addCall(fromFloor = 1, atFloor = 3, DOWN)

      strategy.nextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(2)
      strategy.nextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(3)
      strategy.nextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(4)
      strategy.nextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(5)
      strategy.nextCommand(elevator) must be equalTo("OPEN")
      strategy.gos.size must be equalTo(0)
    }

    "not fucking fool me" in {
      elevator.resetToFloor(5)
      elevator.direction = DOWN
      strategy.reset

      strategy.addCall(fromFloor = 5, atFloor = 2, UP)
      strategy.addCall(fromFloor = 5, atFloor = 7, DOWN)

      strategy.nextCommand(elevator) must be equalTo("UP")
      elevator.floor must beEqualTo(6)
      strategy.nextCommand(elevator) must be equalTo("UP")
      elevator.floor must beEqualTo(7)
      strategy.nextCommand(elevator) must be equalTo("OPEN")
      strategy.nextCommand(elevator) must be equalTo("CLOSE")
      strategy.nextCommand(elevator) must be equalTo("DOWN")
      elevator.floor must beEqualTo(6)
      strategy.nextCommand(elevator) must be equalTo("DOWN")
      elevator.floor must beEqualTo(5)
      strategy.nextCommand(elevator) must be equalTo("DOWN")
      elevator.floor must beEqualTo(4)
      strategy.nextCommand(elevator) must be equalTo("DOWN")
      elevator.floor must beEqualTo(3)
      strategy.nextCommand(elevator) must be equalTo("DOWN")
      elevator.floor must beEqualTo(2)
      strategy.nextCommand(elevator) must be equalTo("OPEN")
      strategy.nextCommand(elevator) must be equalTo("CLOSE")
      strategy.nextCommand(elevator) must be equalTo("UP")
    }

    "check if needs to up" in {
      strategy.needsToGoUp(5, 7) must beTrue
      strategy.needsToGoUp(5, 6) must beTrue
      strategy.needsToGoUp(5, 5) must beFalse
      strategy.needsToGoUp(5, 4) must beFalse
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

  }


}
