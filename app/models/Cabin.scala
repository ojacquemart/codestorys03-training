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

  def beforeNextCommand() = {
    travelers.onNextCommand(floor)
  }

  def nextCommand() = {
    strategy.nextCommand(this)
  }

  def canStop(): Boolean = {
    Logger("USERS").debug(s"CABIN $index - Max cabin size = $size")

    if (travelers.isEmpty) {
      val waitersAt = hasWaitersAt()
      Logger("USERS").debug(s"CABIN $index - No travelers, waiters are at $floor: $waitersAt")
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
      if (canStopForTravelers()) {
        Logger("CABIN").debug("Stop travelers and update points")
        travelers.stopTravelersAt(floor)
      }
    }
    canStop
  }

  def canStopForTravelers(): Boolean = {
    val nbTravelersAt = travelers.users.count(_.isTravelingAt(floor))
    Logger("USERS").debug(s"$nbTravelersAt travelers at $floor")

    nbTravelersAt > 0
  }

  def canStopForWaiters() = {
    val canStopForWaiters = hasWaitersAtInDirection() && remainsPlaceForNewTravelers()
    Logger("USERS").debug(s"CABIN $index - an stop for waiters at $floor to $direction: $canStopForWaiters")

    canStopForWaiters
  }

  def hasWaitersAtInDirection(): Boolean = {
    val nbWaitersAtInDirection = waiters.users.count(_.isWaitingAtInDirection(floor, direction))
    Logger("USERS").debug(s"Has waiters at $floor to $direction: $nbWaitersAtInDirection")

    nbWaitersAtInDirection > 0
  }

  def remainsPlaceForNewTravelers(): Boolean = {
    val remainsPlace = size > 0 && travelers.size < size
    Logger("USERS").debug(s"Remains place for new travelers: $remainsPlace")

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
