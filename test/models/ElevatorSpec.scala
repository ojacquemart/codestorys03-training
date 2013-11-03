package models

import org.specs2.mutable.Specification
import org.specs2.matcher.MatchResult

object ElevatorSpec extends Specification {

  val MaxFloor = 6
  val elevator = new SimpleElevator(MaxFloor, new UpAndDownStrategy())

  "Elevator" should {
    def assertDefaultElevator: MatchResult[Boolean] = {
      elevator.floor should be equalTo (0)
      elevator.direction should be equalTo (UP)
      elevator.opened should beFalse
    }

    "get default values" in {
      assertDefaultElevator
    }

    "reset values" in {
      elevator.floor = 1
      elevator.direction = DOWN
      elevator.opened = true

      elevator.reset

      assertDefaultElevator
    }

    "check if is at bottom floor" in {
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
