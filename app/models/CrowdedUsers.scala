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



}
