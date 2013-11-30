package models

import org.specs2.mutable.Specification
import org.specs2.specification.Scope

object CommandSpec extends Specification {

  val HigherFloor = 19

  class WithCabin(lowerFloor: Int = 0, higherFloor: Int = HigherFloor, index: Int = 0, size: Int = 10, floor: Int = 0) extends Scope {
    val cabin = Cabin(index, lowerFloor, higherFloor, size)
    cabin.floor = floor
  }

  "Nothing command" should {
    "do nothing" in new WithCabin {
      NothingCommand.to(cabin) must be equalTo("NOTHING")
    }
  }

  "Up command" should {

    "do nothing when elevator is at top floor" in new WithCabin(floor = HigherFloor) {
      UpCommand.to(cabin) must be equalTo("NOTHING")
    }

    "go up one floor in UP direction" in new WithCabin(floor = 1) {
      cabin.direction = DOWN
      UpCommand.to(cabin) must be equalTo("UP")
      cabin.floor must be equalTo(2)
      cabin.direction must be equalTo(UP)
    }
  }


  "Down command" should {

    "do nothing when elevator is at bottom floor" in new WithCabin(floor = 0) {
      cabin.floor = 0
      DownCommand.to(cabin) must be equalTo("NOTHING")
    }

    "go down one floor in DOWN direction" in new WithCabin(floor = 2) {
      cabin.direction = UP
      DownCommand.to(cabin) must be equalTo("DOWN")
      cabin.floor must be equalTo(1)
      cabin.direction must be equalTo(DOWN)
    }
  }

  "Open command" should {

    "do nothing when doors are already opened" in new WithCabin {
      cabin.door = Door.OPEN
      OpenCommand.to(cabin) must be equalTo("NOTHING")
      cabin.door must be equalTo(Door.OPEN)
    }

    "open doors if doors are closed at bottom" in new WithCabin {
      cabin.door = Door.CLOSE
      OpenCommand.to(cabin) must be equalTo("OPEN")
      cabin.door must be equalTo(Door.OPEN)
    }

    "open doors if doors are closed at top" in new WithCabin {
      cabin.door = Door.CLOSE
      OpenCommand.to(cabin) must be equalTo("OPEN")
      cabin.door must be equalTo(Door.OPEN)
    }

    "open doors if doors are closed indicating current direction if remains travelers in the current direction" in new WithCabin(floor = 10) {
      cabin.door = Door.CLOSE
      cabin.travelers.addTraveler(1, 5)
      OpenCommand.to(cabin) must be equalTo("OPEN")
      cabin.door must be equalTo(Door.OPEN)

      cabin.travelers.addTraveler(1, 11)
      cabin.door = Door.CLOSE
      OpenCommand.to(cabin) must be equalTo("OPEN_UP")
      cabin.door must be equalTo(Door.OPEN)

      cabin.direction = DOWN
      cabin.door = Door.CLOSE
      OpenCommand.to(cabin) must be equalTo("OPEN_DOWN")
      cabin.door must be equalTo(Door.OPEN)
    }
  }

  "Close command" should {

    "do nothing when doors are already closed" in new WithCabin {
      cabin.door = Door.CLOSE
      CloseCommand.to(cabin) must be equalTo("NOTHING")
      cabin.door must be equalTo(Door.CLOSE)
    }

    "close doors if doors are opened" in new WithCabin {
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
