package models

import scala.math._
import scala.collection.mutable.MutableList

import play.api.Logger

class Users extends Reset {
  
  var users = emptyMutableUsers

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
   *
   * @param floor the current floor
   * @param nextFloorsToGo the next floors to update.
   *
   * @return an empty Mutable list.
   */
  def updateNextFloorsToGo(floor: Int, nextFloorsToGo: MutableList[Int]): MutableList[Int] = {
    Logger.debug(s"@@@ Add next floors... $nextFloorsToGo")
    nextFloorsToGo.foreach(nextFloor => {
      flagNextToFloorToDefine(floor)
      goToFloor(nextFloor)
    })
    MutableList()
  }

  def travelers = users.filter(_.isTraveling())
  def waiters = users.filter(_.isWaiting())
  def doners = users.filter(_.isDone())

  def donerScores: Int = doners.map(_.score()).sum

  // When user has entered
  def flagNextToFloorToDefine(floor: Int) = {
    users
      .find(u => u.fromFloor == floor && u.isToFloorStateUndefined)
      .map(_.setToFloorStateToNextToDefine)
  }

  // When user goes to a floor
  def goToFloor(floor: Int) = {
    users
      .find(_.isNextToFloorStateToNextToDefine)
      .map(_.setupToFloor(floor))
  }

  def stopTravelersAt(floor: Int) = {
    users.filter(_.isTraveling()).foreach(_.stopTravelAt(floor))
  }

  // Check if can stop at floor and direction for user
  def canStopAt(floor: Int, to: Direction): Boolean = {
    if (hasNoTravelers()) {
      val waitersAt = hasWaitersAt(floor)
      Logger.debug(s"No travelers, waiters are at $floor: $waitersAt")
      waitersAt
    }
    else {
      val canStopForTravelerOrWaiterAt = canStopAtForTravelerOrWaiterAt(floor, to)
      if (canStopForTravelerOrWaiterAt) return true

      val canStopForLoserAt = hasTravelersAtAndLosing(floor)
      Logger.debug(s"Only losers traveling are at $floor to $to: $canStopForLoserAt")
      return canStopForLoserAt
    }
  }

  def canStopAtForTravelerOrWaiterAt(floor: Int, to: Direction) = {
    Logger.debug(s"Travelers ${users.filter(_.state == UserState.TRAVELING)}")
    val travelersAtScoring = hasTravelersAtAndScoring(floor)
    val waitersAtInDirection = hasWaitersAtInDirection(floor, to)
    Logger.debug(s"Travelers are scoring at $floor: $travelersAtScoring")
    Logger.debug(s"Waiters are at $floor in direction $to: $waitersAtInDirection")
    val canStop = travelersAtScoring || waitersAtInDirection
    Logger.debug(s"\tCan stop for traveler or waiter at $floor in direction $to: $canStop")

    canStop
  }

  def hasNoTravelers() = users.count(_.isTraveling()) == 0
  def hasWaitersAt(floor: Int) = users.count(_.isWaitingAt(floor)) > 0

  def hasTravelersAtAndScoring(floor: Int): Boolean =
    users.count(_.isTravelingAtAndScoring(floor)) > 0

  def hasWaitersAtInDirection(floor: Int, to: Direction) =
    users.count(_.isWaitingAtInDirection(floor, to)) > 0

  def hasTravelersAtAndLosing(floor: Int) = users.count(_.isTravelingAtAndLosing(floor)) > 0

  def tick() = users.foreach(_.tick())
  def checkStateAt(floor: Int) = users.foreach(_.stopTravelAt(floor))
  def removeDone() = users = users.filterNot(_.isDone)

  def getDirectionTypeForTravelers(floor: Int, to: Direction) = {
    val (travelersInDirection, travelersInInverseDirection) =
      users.filter(_.isTraveling()).partition(u => u.directionByFloor(floor) == to)

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
        .sortBy(_.toFloor)
        .minBy(waiter => Math.abs(floor - waiter.toFloor))

      Logger.debug(s"Lets go the first waiter: $nearestWaiter")
      val goesUp = nearestWaiter.needsToGoUp(floor)
      Logger.debug(s"\tNeeds to go up: $goesUp")

      if (goesUp) NextDirectionType.TO_UP else NextDirectionType.TO_DOWN
    }
  }

  def reset {
    users = emptyMutableUsers
  }
}

object NextDirectionType extends Enumeration {
  val TO_DEFINE_BY_WAITERS, TO_UP, TO_DOWN, TO_MIDDLE_FLOOR, SAME_AS_CURRENT, INVERSE = Value
}

object UserState  extends Enumeration {
  val WAITING, TRAVELING, DONE = Value
}

object ToFloorState  extends Enumeration {
  val UNDEFINED, NEXT_TO_DEFINE, DEFINED = Value
}

case class User(fromFloor: Int, var toFloor: Int = -1, direction: Direction = UP) {

  import UserState._
  import ToFloorState._

  var state = WAITING
  var waitingTicks = 0
  var travelingTicks = 0

  var toFloorState = UNDEFINED

  def isToFloorStateUndefined = toFloorState == UNDEFINED
  def setToFloorStateToNextToDefine = toFloorState = NEXT_TO_DEFINE
  def setToFloorStateToDefined = toFloorState = DEFINED
  def isNextToFloorStateToNextToDefine = toFloorState == NEXT_TO_DEFINE

  def setupToFloor(floor: Int) = {
    toFloor = floor
    setToFloorStateToDefined
    travel()
  }

  def stopTravelAt(floor: Int) {
    if (isTravelingAt(floor)) {
      done()
    }
  }

  def isWaitingAtInDirection(floor: Int, to: Direction) = isWaitingAt(floor) && direction == to
  def isWaitingAt(floor: Int) = isWaiting() && fromFloor == floor
  def isWaiting() = state == WAITING

  def isTravelingAtAndScoring(floor: Int) = isTravelingAt(floor)// && score > 0
  def isTravelingAt(floor: Int) = isTraveling() && toFloor == floor
  def isTraveling() = state == TRAVELING

  def isTravelingAtAndLosing(floor: Int) = isTravelingAt(floor) && score == 0

  def isDone() = state == DONE

  def travel() = state = TRAVELING
  def done() = state = DONE

  def directionByFloor(floor: Int) = if (floor > toFloor) DOWN else UP
  def needsToGoUp(floor: Int) = floor < toFloor

  def tick() = {
    state match {
      case WAITING => waitingTicks += 1
      case TRAVELING => travelingTicks += 1
      case _ =>
    }
  }

  def score() = {
    val waitingTime = waitingTicks / 2
    val distance = abs(fromFloor - toFloor)
    val travelingTime = travelingTicks

    val points = 20 + 2 + distance - waitingTime - travelingTime
    min(max(0, points), 20)
  }

  override def toString() = {
    s"User(from=$fromFloor, to=$toFloor, direction=$direction, points=$score)"
  }

}
