package models

import scala.collection.mutable.MutableList

import play.api.Logger

class Users(var maxTravelers: Int = 30) extends Reset {

  var users: MutableList[User] = emptyMutableUsers

  def emptyMutableUsers = MutableList[User]()

  def size = users.size

  // for testing
  def add(floor: Int, toFloor: Int, to: Direction, state: UserState.Value) = {
    val user = User(fromFloor = floor, toFloor = toFloor, direction = to)
    user.state = state

    users += user
  }

  def add(floor: Int, to: Direction) = {
    val user = User(fromFloor = floor, direction = to)
    users += user
  }

  /**
   * Update the toFloor for users who were waiting and just entered in the cabin.
   */
  def updateNextFloorsToGo(floor: Int, nextFloorsToGo: MutableList[NextFloor]) = {
    Logger.debug(s"@@@ Add next floors... $nextFloorsToGo")
    nextFloorsToGo.foreach(nextFloor => {
      flagNextToFloorToDefine(floor, nextFloor.cabin)
      goToFloor(nextFloor)
    })
  }

  def travelersSize = users.count(_.isTraveling())

  def travelers = users.filter(_.isTraveling())

  def waiters = users.filter(_.isWaiting())

  def waitersSize = users.count(_.isWaiting())

  def doners = users.filter(_.isDone())

  def donerScores: Int = doners.map(_.score()).sum

  // When user has entered
  def flagNextToFloorToDefine(floor: Int, cabin: Int) = {
    users
      .find(u => u.cabin == cabin && u.fromFloor == floor && u.isToFloorStateUndefined)
      .map(_.setToFloorStateToNextToDefine)
  }

  // When user goes to a floor
  def goToFloor(nextFloor: NextFloor) = {
    users
      .find(_.isNextToFloorStateToNextToDefine)
      .map(_.setupToFloor(nextFloor))
  }

  def stopTravelersAt(floor: Int) = {
    users.filter(_.isTraveling()).foreach(_.stopTravelAt(floor))
  }

  // Check if can stop at floor and direction for user
  def canStopAt(floor: Int, to: Direction): Boolean = {
    Logger.debug(s"Max cabin size = $maxTravelers")

    if (hasNoTravelers()) {
      val waitersAt = hasWaitersAt(floor)
      Logger.debug(s"No travelers, waiters are at $floor: $waitersAt")
      waitersAt
    } else if (canStopForTravelersOrWaitersAt(floor, to)) true
    else {
      val canStopForLoserAt = hasTravelersAtAndLosing(floor)
      Logger.debug(s"Only losers traveling are at $floor to $to: $canStopForLoserAt")
      canStopForLoserAt
    }
  }

  def hasWaitersAt(floor: Int) = users.count(_.isWaitingAt(floor)) > 0

  def canStopForTravelersOrWaitersAt(floor: Int, to: Direction) = {
    val canStop = hasTravelersAt(floor) || canStopForWaitersAt(floor, to)
    Logger.debug(s"\tCan stop for traveler or waiter at $floor in direction $to: $canStop")

    canStop
  }

  def canStopForWaitersAt(floor: Int, to: Direction) = {
    val canStopForWaiters = hasWaitersAtInDirection(floor, to) && remainsPlaceForNewTravelers()
    Logger.debug(s"Can stop for waiters at $floor to $to: $canStopForWaiters")

    canStopForWaiters
  }

  def hasTravelersAtAndLosing(floor: Int) = users.count(_.isTravelingAtAndLosing(floor)) > 0

  def remainsPlaceForNewTravelers(): Boolean = {
    val remainsPlace = travelersSize < maxTravelers
    Logger.debug(s"Remains place for new travelers: $remainsPlace")

    remainsPlace
  }

  // TODO: use isTravelingAtAndScoring to improve score when a large number of users.
  def hasTravelersAt(floor: Int): Boolean = {
    val nbTravelersAt = users.count(_.isTravelingAt(floor))
    Logger.debug(s"$nbTravelersAt travelers at $floor")

    nbTravelersAt > 0
  }
  def hasNoTravelers() = users.count(_.isTraveling()) == 0


  def hasWaitersAtInDirection(floor: Int, to: Direction): Boolean = {
    val nbWaitersAtInDirection = users.count(_.isWaitingAtInDirection(floor, to))
    Logger.debug(s"Has waiters at $floor to $to: $nbWaitersAtInDirection")

    nbWaitersAtInDirection > 0
  }

  def onNextCommand(floor: Int, nextFloors: MutableList[NextFloor]) = {
    updateNextFloorsToGo(floor, nextFloors)
    tick(floor)
    removeDone()
  }

  def tick(floor: Int) = users.foreach(_.tick(floor))

  def removeDone() = users = users.filterNot(_.isDone)

  def getDirectionTypeForTravelers(floor: Int, to: Direction) = {
    val (travelersInDirection, travelersInInverseDirection) =
      users.filter(_.isTraveling()).partition(u => u.directionFromToFloorByFloor(floor) == to)

    Logger.debug(s"Travelers in direction: ${travelersInDirection.size}")
    Logger.debug(s"Travelers in inverse direction: ${travelersInInverseDirection.size}")

    if (travelersInDirection.size > 0) NextDirectionType.SAME_AS_CURRENT
    else if (travelersInInverseDirection.size > 0) NextDirectionType.INVERSE
    else NextDirectionType.TO_DEFINE_BY_WAITERS
  }

  def getDirectionTypeForWaiters(floor: Int, to: Direction) = {
    val waiters = users.filter(_.isWaiting())
    if (waiters.size == 0) {
      Logger.debug("No waiters, force direction to middle floor")
      NextDirectionType.TO_MIDDLE_FLOOR
    }
    else {
      val nearestWaiter = waiters
        .sortBy(w => w.fromFloor + w.waitingTime)
        .minBy(waiter => Math.abs(floor - waiter.fromFloor))

      Logger.debug(s"Lets go the first waiter: $nearestWaiter")
      val directionToWaiter = nearestWaiter.needsToGoUpFromFloorToFloor(floor)
      Logger.debug(s"\tNeeds to go up $directionToWaiter from $floor to ${nearestWaiter.fromFloor}")

      if (directionToWaiter) NextDirectionType.TO_UP else NextDirectionType.TO_DOWN
    }
  }

  def waitersByFloor() = {
    val byFloor: (User) => Int = u => u.fromFloor
    group(waiters, byFloor)
  }
  def travelersByFloor() = {
    val toFloor: (User) => Int = u => u.toFloor
    group(travelers, toFloor)
  }

  def group(list: MutableList[User], f: (User) => Int) = {
    list.groupBy(f)
      .map(u => (u._1, u._2.size))
      .toList
      .sortBy(_._1)
      .map(u => UserByFloor(u._1, u._2))
  }

  def resetUsers(maxTravelers: Int) = {
    this.maxTravelers = maxTravelers
    reset
  }

  def reset {
    users = emptyMutableUsers
  }

}

object NextDirectionType extends Enumeration {
  val TO_DEFINE_BY_WAITERS, TO_UP, TO_DOWN, TO_MIDDLE_FLOOR, SAME_AS_CURRENT, INVERSE = Value
}

