package models

import play.api.Logger

case class Cabin(val index: Int = 0, var lowerFloor: Int, var higherFloor: Int, val size: Int) {
  val strategy = new OpenCloseStrategy()

  var waiters = new Users
  val travelers = new Users

  var floor = 0
  val middleFloor = if (index % 2 == 0) higherFloor / 2 else (Math.abs(lowerFloor) / 2)

  var direction: Direction = UP
  var door = Door.CLOSE

  def needsToInverseDirection(): Boolean = (direction == UP && isAtTop) || (direction == DOWN && isAtBottom)

  def isAtTop: Boolean = floor == higherFloor

  def isAtBottom: Boolean = floor == lowerFloor

  def isAtMiddle: Boolean = floor == middleFloor

  def isDoorOpened = door == Door.OPEN

  def isDoorClosed = door == Door.CLOSE

  def canDoNothing() = isAtMiddle && isEmpty()

  def isEmpty() = travelers.size == 0 && waiters.size == 0

  def isFull() = travelers.size == size

  def remainsTravelersInCurrentDirection(): Boolean = {
    val remainsTravelers = travelers.users.count(t => if (direction == UP) t.toFloor > floor else floor > t.toFloor) > 0
    Logger.debug(s"Travelers in current direction $remainsTravelers")
    remainsTravelers
  }

  def removeHeadWaiter(): Unit = {
    def removeMaybeWaiter(maybeWaiter: Option[User]) = {
      if (maybeWaiter.isDefined) waiters.users -= maybeWaiter.get
    }

    val waitersAtFloor = waiters.users.filter(_.fromFloor == floor)
    if (waitersAtFloor.size > 1) {
      // More than one waiter at the floor, just take the first in the current direction
      val waitersAtFloorInCurrentDirection = waitersAtFloor.filter(_.direction == direction)
      if (waitersAtFloorInCurrentDirection.size > 0) {
        removeMaybeWaiter(waitersAtFloor.find(_.direction == direction))
      } else {
        removeMaybeWaiter(waitersAtFloor.find(_.fromFloor == floor))
      }
    } else {
      removeMaybeWaiter(waiters.users.find(_.fromFloor == floor))
    }
  }

  def beforeNextCommand() = {
    travelers.onNextCommand(floor)
  }

  def nextCommand() = {
    strategy.nextCommand(this)
  }

  def canStop(): Boolean = {
    Logger.debug(s"CABIN $index - Max cabin size = $size")

    if (travelers.isEmpty) {
      val waitersAt = hasWaitersAt()
      Logger.debug(s"CABIN $index - No travelers, waiters are at $floor: $waitersAt")
      waitersAt
    } else {
      canStopForTravelersOrWaiters()
    }
  }

  def hasWaitersAt() = waiters.hasWaitersAt(floor)

  def canStopForTravelersOrWaiters() = {
    val stopForTravelers = canStopForTravelers()
    val canStop = stopForTravelers || canStopForWaiters()
    if (canStop) {
      if (stopForTravelers) {
        Logger.debug(s"CABIN $index - Stop travelers and update points")
        travelers.stopTravelersAt(floor)
      }
    }
    canStop
  }

  def canStopForTravelers(): Boolean = {
    val nbTravelersAt = travelers.users.count(_.isTravelingAt(floor))
    Logger.debug(s"CABIN $index - $nbTravelersAt travelers at $floor")

    nbTravelersAt > 0
  }

  def canStopForWaiters() = {
    val canStopForWaiters = hasWaitersAtInDirection() && remainsPlaceForNewTravelers()
    Logger.debug(s"CABIN $index - can stop for waiters at $floor to $direction: $canStopForWaiters")

    canStopForWaiters
  }

  def hasWaitersAtInDirection(): Boolean = {
    val nbWaitersAtInDirection = waiters.users.count(_.isWaitingAtInDirection(floor, direction))
    Logger.debug(s"CABIN $index - Has waiters at $floor to $direction: $nbWaitersAtInDirection")

    nbWaitersAtInDirection > 0
  }

  def remainsPlaceForNewTravelers(): Boolean = {
    val remainsPlace = size > 0 && travelers.size < size
    Logger.debug(s"CABIN $index - Remains place for new travelers: $remainsPlace")

    remainsPlace
  }

  def getDirectionTypeForTravelers(): NextDirectionType.Value = {
    travelers.getDirectionTypeForTravelers(floor, direction)
  }

  def getDirectionTypeForWaiters(): NextDirectionType.Value = {
    waiters.getDirectionTypeForWaiters(floor, direction)
  }

  def waitersByFloor() = {
    val byFloor: (User) => Int = u => u.fromFloor
    waiters.group(byFloor)
  }

  def travelersByFloor() = {
    val toFloor: (User) => Int = u => u.toFloor
    travelers.group(toFloor)
  }

}
