package models

import org.specs2.mutable.Specification

object CabinSpec extends Specification {

  val MaxFloor = 19
  val MaxCabinSize = 100
  var cabin = new Cabin(index = 0, lowerFloor = 0, higherFloor = MaxFloor, size = MaxCabinSize)

  def resetCabin(lowerFloor: Int, higherFloor: Int = MaxFloor) = {
    val cabin = new Cabin(0, lowerFloor, higherFloor, MaxCabinSize)
    cabin.floor = lowerFloor
    cabin
  }
  
  "Cabin" should {

    "get default values" in {
      cabin.floor should be equalTo (0)
      cabin.direction should be equalTo (UP)
      cabin.door should be equalTo(Door.CLOSE)
    }

    "reset values" in {
      cabin.direction = DOWN
      cabin.floor = 1
      cabin.door = Door.OPEN
      cabin.users.add(1, DOWN)

      cabin = resetCabin(0)

      cabin.floor should be equalTo (0)
      cabin.users.size must be equalTo(0)
      cabin.direction should be equalTo (UP)
      cabin.door must be equalTo(Door.CLOSE)
    }

    "check if is at bottom floor" in {
      cabin = resetCabin(0)
      
      cabin.floor = 0
      cabin.isAtBottom must beTrue
      cabin.floor = 1
      cabin.isAtBottom must beFalse
    }

    "check if is at top floor" in {
      cabin = resetCabin(0)
      cabin.isAtTop must beFalse
      cabin.floor = MaxFloor
      cabin.isAtTop must beTrue
    }

    "check if is at the middle floor" in {
      cabin = resetCabin(0)
      cabin.isAtMiddle must beFalse
      cabin.floor = 10
      cabin.isAtMiddle must beTrue

      cabin = resetCabin(-13, 27)
      cabin.floor = 8
      cabin.isAtMiddle must beTrue
    }

    "check if needs to change current direction" in {
      cabin = resetCabin(-3)
      cabin.lowerFloor = -3
      cabin.floor = 1
      cabin.direction = UP
      cabin.needsToInverseDirection() must beFalse
      cabin.floor = MaxFloor
      cabin.needsToInverseDirection() must beTrue
      cabin.direction = DOWN
      cabin.floor = 0
      cabin.needsToInverseDirection() must beFalse
      cabin.floor = cabin.lowerFloor
      cabin.needsToInverseDirection() must beTrue
    }

    "check if door is opened" in {
      cabin.door = Door.OPEN
      cabin.isDoorOpened must beTrue
      cabin.door = Door.CLOSE
      cabin.isDoorOpened must beFalse
    }

    "check if door is closed" in {
      cabin.door = Door.CLOSE
      cabin.isDoorClosed must beTrue
      cabin.door = Door.OPEN
      cabin.isDoorClosed must beFalse
    }

    "can do nothing when at middle floor" in {
      cabin = resetCabin(0)
      cabin.floor = 10
      cabin.canDoNothing() must beTrue
    }

  }


}
