package models

import org.specs2.mutable.Specification
import org.specs2.specification.Scope

object CabinSpec extends Specification {

  val HigherFloor = 19
  val MaxCabinSize = 100

  class WithCabin(lowerFloor: Int = 0, higherFloor: Int = HigherFloor, index: Int = 0, size: Int = MaxCabinSize, floor: Int = 0) extends Scope {
    val cabin = Cabin(index, lowerFloor, higherFloor, size)
    cabin.floor = floor
  }

  def resetCabin(lowerFloor: Int, higherFloor: Int = HigherFloor, index: Int = 0) = {
    val cabin = new Cabin(index, lowerFloor, higherFloor, MaxCabinSize)
    cabin.floor = lowerFloor
    cabin
  }
  
  "Cabin" should {

    "get default values" in new WithCabin {
      cabin.floor should be equalTo (0)
      cabin.direction should be equalTo (UP)
      cabin.door should be equalTo(Door.CLOSE)
    }

    "check if is at bottom floor" in new WithCabin {
      cabin.floor = 0
      cabin.isAtBottom must beTrue
      cabin.floor = 1
      cabin.isAtBottom must beFalse
    }

    "check if is at top floor" in new WithCabin {
      cabin.isAtTop must beFalse
      cabin.floor = HigherFloor
      cabin.isAtTop must beTrue
    }

    "check if is at the middle floor when one cabin" in new WithCabin {
      cabin.isAtMiddle must beFalse
        cabin.floor = 9
        cabin.isAtMiddle must beTrue
    }

    "check if is at the middle floor when first cabin middle is around highler floor" in new WithCabin(lowerFloor = -13, higherFloor = 27, index = 0) {
        cabin.floor = 13
        cabin.isAtMiddle must beTrue
      }

    "check if is at the middle floor when second cabin middle is around lower floor" in new WithCabin(lowerFloor = -13, higherFloor = 27, index = 1) {
      cabin.floor = 6
      cabin.isAtMiddle must beTrue
    }

    "check if needs to change current direction" in new WithCabin(lowerFloor = -3, floor = 1) {
      cabin.direction = UP
      cabin.needsToInverseDirection() must beFalse
      cabin.floor = HigherFloor
      cabin.needsToInverseDirection() must beTrue
      cabin.direction = DOWN
      cabin.floor = 0
      cabin.needsToInverseDirection() must beFalse
      cabin.floor = cabin.lowerFloor
      cabin.needsToInverseDirection() must beTrue
    }

    "check if door is opened" in new WithCabin {
      cabin.door = Door.OPEN
      cabin.isDoorOpened must beTrue
      cabin.door = Door.CLOSE
      cabin.isDoorOpened must beFalse
    }

    "check if door is closed" in new WithCabin {
      cabin.door = Door.CLOSE
      cabin.isDoorClosed must beTrue
      cabin.door = Door.OPEN
      cabin.isDoorClosed must beFalse
    }

    "check if remains travelers in current direction" in new WithCabin(floor = 5) {
      cabin.direction = UP
      cabin.travelers.addTraveler(1, 4)
      cabin.travelers.addTraveler(1, 3)
      cabin.travelers.addTraveler(0, 4)
      cabin.remainsTravelersInCurrentDirection() must beFalse

      cabin.direction = DOWN
      cabin.remainsTravelersInCurrentDirection() must beTrue

      cabin.direction = UP
      cabin.travelers.addTraveler(6, 8)
      cabin.travelers.addTraveler(10, 12)
      cabin.remainsTravelersInCurrentDirection() must beTrue
    }

    "can do nothing when at middle floor" in new WithCabin {
      cabin.floor = 9
      cabin.canDoNothing() must beTrue
    }

    "dont stop when no waiter or traveler at a floor" in new WithCabin {
      cabin.canStop must beFalse
      cabin.floor = 10
      cabin.canStop must beFalse
    }

    "remove head waiter" in new WithCabin(floor = 6) {
      cabin.direction = UP
      cabin.waiters.addWaiter(6, UP)
      cabin.waiters.addWaiter(6, DOWN)
      cabin.waiters.addWaiter(6, UP)
      cabin.removeHeadWaiter()
      cabin.removeHeadWaiter()
      cabin.waiters.size must be equalTo(1)
      cabin.waiters.users.head.direction must be equalTo(DOWN)

      cabin.direction = DOWN
      cabin.removeHeadWaiter()
      cabin.waiters.size must be equalTo(0)
    }

    "stop when has travelers or waiters in a direction at a floor" in new WithCabin {
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

    "don't stop when max travelers is reached" in new WithCabin {
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
