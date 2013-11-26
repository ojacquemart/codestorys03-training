package models

import play.api.Logger
import scala.collection.mutable

case class Cabin(val index: Int = 0, var lowerFloor: Int, var higherFloor: Int, val size: Int) {
  val users = new Users(size)
  val strategy = new OpenCloseStrategy()

  var floor = 0
  def middleFloor = ((lowerFloor + higherFloor) / 2) + 1

  var direction: Direction = UP
  var door = Door.CLOSE

  def needsToInverseDirection(): Boolean = (direction == UP && isAtTop) || (direction == DOWN && isAtBottom)

  def isAtTop: Boolean = floor == higherFloor
  def isAtBottom: Boolean = floor == lowerFloor
  def isAtMiddle: Boolean = floor == middleFloor

  def isDoorOpened = door == Door.OPEN
  def isDoorClosed = door == Door.CLOSE

  def canDoNothing() = isAtMiddle && isEmpty()
  def isEmpty() = users.size == 0

  def canStopAt(floor: Int, to: Direction): Boolean = {
    users.canStopAt(floor, to)
  }

  def beforeNextCommand(nextFloors: mutable.MutableList[NextFloor]) = {
    users.onNextCommand(floor, nextFloors)
  }
  
  def nextCommand() = {
    strategy.nextCommand(this)
  }

  def canStop() = {
    val canStop = users.canStopAt(floor, direction)
    if (canStop) {
      Logger.debug("Stop travelers and update points")
      users.stopTravelersAt(floor)
    }
    canStop
  }

  def getDirectionTypeForTravelers(): NextDirectionType.Value = {
    users.getDirectionTypeForTravelers(floor, direction)
  }

  def getDirectionTypeForWaiters(): NextDirectionType.Value = {
    users.getDirectionTypeForWaiters(floor, direction)
  }

}
