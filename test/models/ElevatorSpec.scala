package models

import org.specs2.mutable.Specification

object ElevatorSpec extends Specification {

  val MaxFloor = 20
  val elevator = new SimpleElevator(MaxFloor, new UpAndDownStrategy())

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
  }

  "UpAndDownStrategy" should {

    val upAndDownStrategy = new UpAndDownStrategy()

    "get UP command from UP direction" in {
      upAndDownStrategy.fromDirection(UP) must be equalTo(UpCommand)
    }

    "get DOWN command from DOWN direction" in {
      upAndDownStrategy.fromDirection(DOWN) must be equalTo(DownCommand)
    }

    "get DOWN command when at top floor" in {
      elevator.floor = MaxFloor - 1
      elevator.direction = UP
      upAndDownStrategy.getNextCommand(elevator) must be equalTo("DOWN")
    }

    "get UP command up when at bottom floor" in {
      elevator.floor = 0
      elevator.direction = DOWN
      upAndDownStrategy.getNextCommand(elevator) must be equalTo("UP")
    }
  }
  
  "WithStopStrategy" should {
    
    val withStopStrategy = new WithStopStrategy()
    val OneFloorUpStop = new Stop(1, 1, UP)
    
    "add a stop" in {
      withStopStrategy.stops.size must be equalTo(0)
      withStopStrategy.addStop(OneFloorUpStop)
      withStopStrategy.stops.size must be equalTo(1)
    }
    
    "handle a stop in opening doors and closing doors" in {
      elevator.floor = 1
      withStopStrategy.addStop(OneFloorUpStop)

      withStopStrategy.getNextCommand(elevator) must be equalTo("OPEN")
      withStopStrategy.stops.size must be equalTo(0)

      withStopStrategy.getNextCommand(elevator) must be equalTo("CLOSE")
      withStopStrategy.getNextCommand(elevator) must be equalTo("UP")
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

  "Direction" should {

    "inverse current direction" in {
      UP.inverse should be equalTo(DOWN)
      DOWN.inverse should be equalTo(UP)
    }
  }

}
