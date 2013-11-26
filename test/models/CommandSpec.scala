package models

import org.specs2.mutable.Specification

object CommandSpec extends Specification {

  val HigherFloor = 19
  val cabin = new Cabin(0, lowerFloor = 0, higherFloor = HigherFloor, size = 10)

  "Nothing command" should {
    "do nothing" in {
      NothingCommand.to(cabin) must be equalTo("NOTHING")
    }
  }

  "Up command" should {

    "do nothing when elevator is at top floor" in {
      cabin.floor = HigherFloor
      UpCommand.to(cabin) must be equalTo("NOTHING")
    }

    "go up one floor in UP direction" in {
      cabin.floor = 1
      cabin.direction = DOWN
      UpCommand.to(cabin) must be equalTo("UP")
      cabin.floor must be equalTo(2)
      cabin.direction must be equalTo(UP)
    }
  }


  "Down command" should {

    "do nothing when elevator is at bottom floor" in {
      cabin.floor = 0
      DownCommand.to(cabin) must be equalTo("NOTHING")
    }

    "go down one floor in DOWN direction" in {
      cabin.floor = 2
      cabin.direction = UP
      DownCommand.to(cabin) must be equalTo("DOWN")
      cabin.floor must be equalTo(1)
      cabin.direction must be equalTo(DOWN)
    }
  }

  "Open command" should {

    "do nothing when doors are already opened" in {
      cabin.door = Door.OPEN
      OpenCommand.to(cabin) must be equalTo("NOTHING")
      cabin.door must be equalTo(Door.OPEN)
    }

    "open doors if doors are closed" in {
      cabin.door = Door.CLOSE
      OpenCommand.to(cabin) must be equalTo("OPEN")
      cabin.door must be equalTo(Door.OPEN)
    }
  }

  "Close command" should {

    "do nothing when doors are already closed" in {
      cabin.door = Door.CLOSE
      CloseCommand.to(cabin) must be equalTo("NOTHING")
      cabin.door must be equalTo(Door.CLOSE)
    }

    "close doors if doors are opened" in {
      cabin.door = Door.OPEN
      CloseCommand.to(cabin) must be equalTo("CLOSE")
      cabin.door must be equalTo(Door.CLOSE)
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
