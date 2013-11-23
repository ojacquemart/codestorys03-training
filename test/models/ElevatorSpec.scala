package models

import org.specs2.mutable.Specification

object ElevatorSpec extends Specification {

  val MaxFloor = 19
  val MaxCabinSize = 100
  val elevator = new SimpleElevator(MaxFloor, MaxCabinSize, new OpenCloseStrategy())

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

      elevator.resetToFloor(0, MaxFloor, MaxCabinSize)

      elevator.floor should be equalTo (0)
      elevator.users.size must be equalTo(0)
      elevator.direction should be equalTo (UP)
      elevator.door must be equalTo(Door.CLOSE)
    }

    "reset lower floor, higherFloor and cabinSize changing default values" in {
      elevator.resetToFloor(-13, MaxFloor, 15)
      elevator.floor must be equalTo(0)
      elevator.lowerFloor must be equalTo(-13)
      elevator.higherFloor must be equalTo(19)
      elevator.cabinSize must be equalTo(15)
    }

    "check if is at bottom floor" in {
      elevator.resetToFloor(0, MaxFloor, 10)
      elevator.floor = 0
      elevator.isAtBottom must beTrue
      elevator.floor = 1
      elevator.isAtBottom must beFalse
    }

    "check if is at top floor" in {
      elevator.isAtTop must beFalse
      elevator.floor = MaxFloor
      elevator.isAtTop must beTrue
    }

    "check if is at the middle floor" in {
      elevator.resetToFloor(0, MaxFloor, 10)
      elevator.isAtMiddle must beFalse
      elevator.floor = 10
      elevator.isAtMiddle must beTrue

      elevator.resetToFloor(-13, 27, 10)
      elevator.floor = 8
      elevator.isAtMiddle must beTrue
    }

    "check if needs to change current direction" in {
      elevator.resetToFloor(-3, MaxFloor, 10)
      elevator.lowerFloor = -3
      elevator.floor = 1
      elevator.direction = UP
      elevator.needsToInverseDirection() must beFalse
      elevator.floor = MaxFloor
      elevator.needsToInverseDirection() must beTrue
      elevator.direction = DOWN
      elevator.floor = 0
      elevator.needsToInverseDirection() must beFalse
      elevator.floor = elevator.lowerFloor
      elevator.needsToInverseDirection() must beTrue
    }

    "check if door is opened" in {
      elevator.door = Door.OPEN
      elevator.isDoorOpened must beTrue
      elevator.door = Door.CLOSE
      elevator.isDoorOpened must beFalse
    }

    "check if door is closed" in {
      elevator.door = Door.CLOSE
      elevator.isDoorClosed must beTrue
      elevator.door = Door.OPEN
      elevator.isDoorClosed must beFalse
    }

    "can do nothing when at middle floor" in {
      elevator.resetToFloor(0, MaxFloor, MaxCabinSize)
      elevator.floor = 10
      elevator.canDoNothing() must beTrue
    }

  }


}
