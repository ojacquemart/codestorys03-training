package models

import scala.collection.mutable.MutableList
import play.api.Logger

class CrowdedUsers(maxTravelers: Int, users: MutableList[User]) extends Users(maxTravelers, users) {

  override def hasWaitersAt(floor: Int) = users.count(_.isWaitingAt(floor)) > 0

  // TODO: use isTravelingAtAndScoring to improve score when a large number of users.
  override def hasTravelersAt(floor: Int): Boolean = {
    val nbTravelersAt = users.count(_.isTravelingAtAndScoring(floor))
    Logger.debug(s"$nbTravelersAt travelers at $floor")

    nbTravelersAt > 0
  }

  override def canStopForWaitersAt(floor: Int, to: Direction) = {
    false
  }

  override def getDirectionTypeForWaiters(floor: Int, to: Direction) = {
    val waiters = users.filter(_.isWaiting())
    if (waiters.size == 0) {
      Logger.debug("No waiters, force direction to middle floor")
      NextDirectionType.TO_MIDDLE_FLOOR
    }
    else {
      val nearestFloorWithLotOfFewWaiters = findFloorWithNearestWaiter(floor)

      Logger.debug(s"Lets go the first waiter at $nearestFloorWithLotOfFewWaiters")

      val directionToWaiter = needsToGoUpFromFloorToFloor(floor, nearestFloorWithLotOfFewWaiters)
      Logger.debug(s"\tNeeds to go up $directionToWaiter from $floor to ${nearestFloorWithLotOfFewWaiters}")

      if (directionToWaiter) NextDirectionType.TO_UP else NextDirectionType.TO_DOWN
    }
  }

  def needsToGoUpFromFloorToFloor(floor1: Int, floor2: Int) = floor1 < floor2

  def findFloorWithNearestWaiter(floor: Int): Int = {
    waiters
      .sortBy(w => w.fromFloor + w.waitingTime)
      .groupBy(_.fromFloor)
      .toList
      .sortBy(_._2.size)
      .head._1
  }


}
