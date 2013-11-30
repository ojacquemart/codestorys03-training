package models

import math._

object UserState  extends Enumeration {
  val WAITING, TRAVELING, DONE = Value
}

case class User(fromFloor: Int, var toFloor: Int = -1, var cabin: Int = 0, direction: Direction = UP) {

  import UserState._

  var state = WAITING
  var waitingTime = 0
  var travelingTime = 0

  def stopTravelAt(floor: Int) {
    if (isTravelingAt(floor)) {
      done()
    }
  }

  var currentFloor: Int = 0

  def isAt(floor: Int) = currentFloor == floor

  def isWaitingAtInDirection(floor: Int, to: Direction) = isWaitingAt(floor) && direction == to
  def isWaitingAt(floor: Int) = isWaiting() && fromFloor == floor
  def isWaiting() = state == WAITING

  def isTravelingAtAndScoring(floor: Int) = isTravelingAt(floor) && score > 0
  def isTravelingAt(floor: Int) = isTraveling() && toFloor == floor
  def isTraveling() = state == TRAVELING

  def isTravelingAtAndLosing(floor: Int) = isTravelingAt(floor) && score == 0

  def travel() = state = TRAVELING
  def done() = state = DONE

  def isDone() = state == DONE

  def needsToGoUpFromFloorToFloor(floor: Int) = floor < fromFloor
  def directionFromToFloorByFloor(floor: Int) = if (floor > toFloor) DOWN else UP

  def tick(floor: Int) = {
    currentFloor = floor
    state match {
      case WAITING => waitingTime += 1
      case TRAVELING => travelingTime += 1
      case _ =>
    }
  }

  def score(): Int = {
    val waitingTimeBy2 = waitingTime / 2
    val distance = abs(fromFloor - toFloor)

    val points = 20 + 2 + distance - waitingTimeBy2 - travelingTime
    min(max(0, points), 20)
  }

  override def toString() = {
    s"(F=$fromFloor,T=$toFloor,D=$direction,S=$score)"
  }

}
