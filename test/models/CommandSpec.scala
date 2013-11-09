package models

import org.specs2.mutable.Specification

object CommandSpec extends Specification {

  val MaxFloor = 20
  val elevator = new SimpleElevator(MaxFloor, new DirectionStrategy())

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

  "Directions" should {

    "get value from" in {
      Directions.valueOf("UP") must be equalTo(UP)
      Directions.valueOf("DOWN") must be equalTo(DOWN)
    }
  }

}
