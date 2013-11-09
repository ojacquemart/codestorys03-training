package models

import org.specs2.mutable.Specification

object ElevatorSpec extends Specification {

  val MaxFloor = 20
  val elevator = new SimpleElevator(MaxFloor, new DirectionStrategy())

  "Elevator" should {

    "get default values" in {
      elevator.floor should be equalTo (0)
      elevator.direction should be equalTo (UP)
      elevator.opened should beFalse
    }

    "reset values" in {
      elevator.floor = 1
      elevator.direction = DOWN
      elevator.opened = true

      elevator.reset(0)

      elevator.floor should be equalTo (0)
      elevator.direction should be equalTo (UP)
      elevator.opened should beFalse
    }

    "reset lower floor" in {
      elevator.floor = 2
      elevator.reset(10)
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
      elevator.addUser
      elevator.users must be equalTo(1)
      elevator.removeUser
      elevator.users must be equalTo(0)
    }

    "reset users count" in {
      elevator.reset(0)
      elevator.addUser
      elevator.users must be equalTo(1)

      elevator.reset(0)
      elevator.users must be equalTo(0)
    }
  }

  "DirectionStrategy" should {

    val strategy = new DirectionStrategy()

    "reset stops" in {
      strategy.addGo(1)
      strategy.addCall(Call(1, UP))
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
      strategy.getNextCommand(elevator) must be equalTo("DOWN")
    }

    "get UP command up when at bottom floor" in {
      elevator.floor = 0
      elevator.direction = DOWN
      strategy.getNextCommand(elevator) must be equalTo("UP")
    }

    "not move if no one is in the cabin & at the middle floor" in {
      elevator.reset(10)
      strategy.reset
      strategy.getNextCommand(elevator) must be equalTo("NOTHING")
      strategy.getNextCommand(elevator) must be equalTo("NOTHING")
      strategy.getNextCommand(elevator) must be equalTo("NOTHING")
      strategy.getNextCommand(elevator) must be equalTo("NOTHING")
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
      strategy.addGo(Go(12))
      strategy.findBestDirectionByCurrentDirection(elevator) must be equalTo(UP)
    }

    "find best direction by current direction from up to down" in {
      elevator.floor = 15
      elevator.direction = UP
      strategy.reset
      strategy.addGo(Go(12))
      strategy.findBestDirectionByCurrentDirection(elevator) must be equalTo(DOWN)
    }

  }

  "OpenCloseStrategy" should {
    
    val strategy = new OpenCloseStrategy()

    "gets stops from floor" in {
      strategy.addGo(Go(10))
      strategy.getStopFromFloor(10).size must be equalTo(1)
    }

    "get calls in current direction when no go" in {
      elevator.reset(5)
      elevator.direction = DOWN
      strategy.reset

      strategy.getCallFromFloorFloorInCurrentDirection(elevator).size must be equalTo(0)
      strategy.addCall(5, DOWN)
      strategy.getCallFromFloorFloorInCurrentDirection(elevator).size must be equalTo(1)
    }

    "not go and up or down continually with current in middle of two calls" in {
      elevator.reset(2)
      strategy.reset
      strategy.addCall(Call(0, UP))
      strategy.addCall(Call(4, UP))
      strategy.getNextCommand(elevator) must be equalTo("DOWN")
      strategy.getNextCommand(elevator) must be equalTo("DOWN")
      strategy.getNextCommand(elevator) must be equalTo("OPEN")
      strategy.getNextCommand(elevator) must be equalTo("CLOSE")
      strategy.getNextCommand(elevator) must be equalTo("UP")
      strategy.getNextCommand(elevator) must be equalTo("UP")
    }

    "change direction is no go and one call" in {
      elevator.reset(5)
      elevator.direction = UP
      strategy.reset
      strategy.addCall(Call(2, DOWN))
      strategy.getNextCommand(elevator) must be equalTo("DOWN")
      strategy.getNextCommand(elevator) must be equalTo("DOWN")
      strategy.getNextCommand(elevator) must be equalTo("DOWN")
      strategy.getNextCommand(elevator) must be equalTo("OPEN")
      strategy.getNextCommand(elevator) must be equalTo("CLOSE")
      strategy.getNextCommand(elevator) must be equalTo("UP")
    }

    "deserves a go and and has to search for calls in higher floors" in {
      elevator.reset(5)
      elevator.direction = UP
      strategy.reset
      strategy.addGo(3)
      strategy.addCall(6, UP)
      strategy.getNextCommand(elevator) must be equalTo("DOWN")
      strategy.getNextCommand(elevator) must be equalTo("DOWN")
      strategy.getNextCommand(elevator) must be equalTo("OPEN")
      strategy.getNextCommand(elevator) must be equalTo("CLOSE")
      strategy.getNextCommand(elevator) must be equalTo("UP")
    }

    "deserves a go and and has to search for calls in lower floors" in {
      elevator.reset(15)
      elevator.direction = UP
      strategy.reset
      strategy.addGo(17)
      strategy.addCall(6, UP)
      strategy.getNextCommand(elevator) must be equalTo("UP")
      strategy.getNextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(17)
      strategy.getNextCommand(elevator) must be equalTo("OPEN")
      strategy.getNextCommand(elevator) must be equalTo("CLOSE")
      strategy.getNextCommand(elevator) must be equalTo("DOWN")
    }

    "not keep two calls in different direction in the system" in {
      elevator.reset(10)
      strategy.reset
      strategy.addCall(Call(10, UP))
      strategy.addCall(Call(10, DOWN))

      strategy.getNextCommand(elevator) must be equalTo("OPEN")
      strategy.calls.size must be equalTo(0)
    }


    "deserves two consecutive gos" in {
      elevator.reset(15)
      elevator.direction = UP
      strategy.reset
      strategy.addGo(17)
      strategy.addGo(18)
      strategy.getNextCommand(elevator) must be equalTo("UP")
      strategy.getNextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(17)
      strategy.getNextCommand(elevator) must be equalTo("OPEN")
      strategy.getNextCommand(elevator) must be equalTo("CLOSE")
      strategy.getNextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(18)
      strategy.getNextCommand(elevator) must be equalTo("OPEN")
      strategy.getNextCommand(elevator) must be equalTo("CLOSE")
      strategy.getNextCommand(elevator) must be equalTo("DOWN")
    }

    "does not need to stop if no go call or" in {
      elevator.reset(1)
      strategy.reset
      strategy.needsStop(elevator) must beFalse
    }

    "force direction to middle when no another call or go in current direction" in {
      elevator.reset(5)

      strategy.reset
      strategy.addCall(4, UP)

      strategy.getNextCommand(elevator) must be equalTo("DOWN")
      strategy.getNextCommand(elevator) must be equalTo("OPEN")
      strategy.getNextCommand(elevator) must be equalTo("CLOSE")
      strategy.getNextCommand(elevator) must be equalTo("UP")
      strategy.getNextCommand(elevator) must be equalTo("UP")
      strategy.getNextCommand(elevator) must be equalTo("UP")
      strategy.getNextCommand(elevator) must be equalTo("UP")
      strategy.getNextCommand(elevator) must be equalTo("UP")
      strategy.getNextCommand(elevator) must be equalTo("UP")
      strategy.getNextCommand(elevator) must be equalTo("NOTHING")

    }

    "stops with a go at a floor" in {
      elevator.reset(1)
      elevator.direction = UP
      strategy.reset

      strategy.addGo(Go(3))
      strategy.getNextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(2)
      strategy.getNextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(3)
      strategy.getNextCommand(elevator) must be equalTo("OPEN")
      strategy.gos.size must be equalTo(0)
      strategy.getNextCommand(elevator) must be equalTo("CLOSE")
      strategy.getNextCommand(elevator) must be equalTo("UP")
    }

    "stops with a a call at a floor in the same direction" in {
      elevator.reset(1)
      elevator.direction = UP
      strategy.reset

      strategy.addCall(Call(3, UP))
      strategy.getNextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(2)
      strategy.getNextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(3)
      strategy.getNextCommand(elevator) must be equalTo("OPEN")
      strategy.calls.size must be equalTo(0)
      strategy.getNextCommand(elevator) must be equalTo("CLOSE")
      strategy.getNextCommand(elevator) must be equalTo("UP")
    }

    "stops with only one call at a floor in the opposite direction" in {
      elevator.reset(1)
      elevator.direction = UP
      strategy.reset

      strategy.addCall(Call(3, DOWN))
      strategy.getNextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(2)
      strategy.getNextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(3)
      strategy.getNextCommand(elevator) must be equalTo("OPEN")
      strategy.calls.size must be equalTo(0)
      strategy.getNextCommand(elevator) must be equalTo("CLOSE")
      strategy.getNextCommand(elevator) must be equalTo("UP")
    }

    "stops with only one call at a floor in the opposite direction and then a go" in {
      elevator.reset(1)
      elevator.direction = UP
      strategy.reset

      strategy.addCall(Call(3, DOWN))
      strategy.getNextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(2)
      strategy.getNextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(3)
      strategy.getNextCommand(elevator) must be equalTo("OPEN")
      strategy.calls.size must be equalTo(0)
      strategy.getNextCommand(elevator) must be equalTo("CLOSE")
      strategy.addGo(2)
      strategy.getNextCommand(elevator) must be equalTo("DOWN")
      strategy.getNextCommand(elevator) must be equalTo("OPEN")
      strategy.getNextCommand(elevator) must be equalTo("CLOSE")
      strategy.getNextCommand(elevator) must be equalTo("UP")
    }

    "handle a go and a call in the same direction" in {
      elevator.reset(1)
      elevator.direction = UP
      strategy.reset

      strategy.addGo(Go(5))
      strategy.addCall(Call(3, UP))

      strategy.getNextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(2)
      strategy.getNextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(3)
      strategy.getNextCommand(elevator) must be equalTo("OPEN")
      strategy.calls.size must be equalTo(0)
      strategy.getNextCommand(elevator) must be equalTo("CLOSE")
      strategy.getNextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(4)
      strategy.getNextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(5)
      strategy.getNextCommand(elevator) must be equalTo("OPEN")
      strategy.gos.size must be equalTo(0)
    }

    "handle a go and a call in the opposite direction" in {
      elevator.reset(1)
      elevator.direction = UP
      strategy.reset

      strategy.addGo(Go(5))
      strategy.addCall(Call(3, DOWN))

      strategy.getNextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(2)
      strategy.getNextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(3)
      strategy.getNextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(4)
      strategy.getNextCommand(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(5)
      strategy.getNextCommand(elevator) must be equalTo("OPEN")
      strategy.gos.size must be equalTo(0)
    }

  }

  "Nothing command" should {
    "do nothing" in {
      NothingCommand.to(elevator) must be equalTo("NOTHING")
    }
  }

  "Up command" should {

    "do nothing when elevator is at top floor" in {
      elevator.floor = MaxFloor - 1
      UpCommand.to(elevator) must be equalTo("NOTHING")
    }

    "go up one floor in UP direction" in {
      elevator.floor = 1
      elevator.direction = DOWN
      UpCommand.to(elevator) must be equalTo("UP")
      elevator.floor must be equalTo(2)
      elevator.direction must be equalTo(UP)
    }
  }


  "Down command" should {

    "do nothing when elevator is at bottom floor" in {
      elevator.floor = 0
      DownCommand.to(elevator) must be equalTo("NOTHING")
    }

    "go down one floor in DOWN direction" in {
      elevator.floor = 2
      elevator.direction = UP
      DownCommand.to(elevator) must be equalTo("DOWN")
      elevator.floor must be equalTo(1)
      elevator.direction must be equalTo(DOWN)
    }
  }

  "Open command" should {

    "do nothing when doors are already opened" in {
      elevator.opened = true
      OpenCommand.to(elevator) must be equalTo("NOTHING")
    }

    "open doors if doors are closed" in {
      elevator.opened = false
      OpenCommand.to(elevator) must be equalTo("OPEN")
      elevator.opened must beTrue
    }
  }

  "Close command" should {

    "do nothing when doors are already closed" in {
      elevator.opened = false
      CloseCommand.to(elevator) must be equalTo("NOTHING")
    }

    "close doors if doors are opened" in {
      elevator.opened = true
      CloseCommand.to(elevator) must be equalTo("CLOSE")
      elevator.opened must beFalse
    }
  }

}
