package models

import org.specs2.mutable.Specification

object CabinSpec extends Specification {

  val MaxFloor = 19
  val MaxCabinSize = 100
  var cabin = new Cabin(index = 0, lowerFloor = 0, higherFloor = MaxFloor, size = MaxCabinSize)

  def resetCabin(lowerFloor: Int, higherFloor: Int = MaxFloor, index: Int = 0) = {
    val cabin = new Cabin(index, lowerFloor, higherFloor, MaxCabinSize)
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
      cabin.travelers.addWaiter(1, DOWN)

      cabin = resetCabin(0)

      cabin.floor should be equalTo (0)
      cabin.travelers.size must be equalTo(0)
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
      cabin.floor = 9
      cabin.isAtMiddle must beTrue

      cabin = resetCabin(-13, 27, index = 0)
      cabin.floor = 13
      cabin.isAtMiddle must beTrue

      cabin = resetCabin(-13, 27, index = 1)
      cabin.floor = 6
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
      cabin.floor = 9
      cabin.canDoNothing() must beTrue
    }

    "dont stop when no waiter or traveler at a floor" in {
      cabin = resetCabin(0)
      cabin.canStop must beFalse

      cabin.floor = 10
      cabin.canStop must beFalse
    }

    "stop when has travelers or waiters in a direction at a floor" in {
      cabin = resetCabin(0)

      cabin.travelers.addTraveler(10, 0)
      cabin.travelers.addTraveler(0, 2)
      cabin.travelers.addTraveler(1, 5)

      cabin.waiters.addWaiter(3, UP)
      cabin.waiters.addWaiter(4, DOWN)
      cabin.waiters.addWaiter(15, DOWN)

      cabin.canStop() must beTrue // traveler exits
      cabin.floor = 1
      cabin.canStop() must beFalse // no waiter/traveler
      cabin.floor = 2
      cabin.canStop() must beTrue // traveler exits
      cabin.floor = 3
      cabin.canStop() must beTrue // waiter enters
      cabin.floor = 4
      cabin.canStop() must beFalse // waiter inverse direction
      cabin.floor = 15
      cabin.canStop() must beFalse // waiter inverse direction

      cabin.direction = DOWN
      cabin.canStop() must beTrue // waiter enters in down direction
    }

    "don't stop when max travelers is reached" in {
      cabin = resetCabin(0)
      for (i <- 0 until cabin.size) cabin.travelers.addTraveler(1, 10)

      cabin.travelers.size must be equalTo(cabin.size)
      cabin.remainsPlaceForNewTravelers() must beFalse

      cabin.waiters.addWaiter(11, UP) // waiter at 11

      cabin.floor = 11
      cabin.canStop() must beFalse // waiter can't enter, cabin is full!
      cabin.floor = 10
      cabin.canStop() must beTrue
      cabin.travelers.removeDone()

      cabin.floor = 11
      cabin.canStop() must beTrue
    }

  }


}
