package models

import scala.collection.mutable.{ArrayBuffer  }

import play.api.Logger

case class StopAt(cabin: Int, floor: Int, to: Direction)

class Users extends Reset {

  var users: ArrayBuffer[User] = emptyMutableUsers

  def emptyMutableUsers = ArrayBuffer[User]()

  def isEmpty = size == 0
  def size = users.size

  // for testing
  def add(floor: Int, toFloor: Int, to: Direction, state: UserState.Value) = {
    val user = User(fromFloor = floor, toFloor = toFloor, direction = to)
    user.state = state

    users += user
  }

  def addTraveler(fromFloor: Int, toFloor: Int) = {
    val user = User(fromFloor, toFloor = toFloor)
    user.travel()

    users += user
  }

  def addWaiter(floor: Int, to: Direction) = {
    val user = User(fromFloor = floor, direction = to)
    users += user
  }

  def stopTravelersAt(floor: Int) = {
    users.filter(_.isTraveling()).foreach(_.stopTravelAt(floor))
  }

  def hasWaitersAt(floor: Int) = users.count(_.isWaitingAt(floor)) > 0

  def onNextCommand(floor: Int) = {
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

  def group(f: (User) => Int) = {
    users.groupBy(f)
      .map(u => (u._1, u._2.size))
      .toList
      .sortBy(_._1)
      .map(u => UserByFloor(u._1, u._2))
  }

  def reset = users = emptyMutableUsers

}

object NextDirectionType extends Enumeration {
  val TO_DEFINE_BY_WAITERS, TO_UP, TO_DOWN, TO_MIDDLE_FLOOR, SAME_AS_CURRENT, INVERSE = Value
}

