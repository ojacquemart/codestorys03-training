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


trait Strategy {
  def nextCommand(cabin: Cabin): String
}

class DirectionStrategy extends Strategy {

  def fromDirection(direction: Direction): Command = {
    if (direction == UP) UpCommand
    else DownCommand
  }

  def nextCommand(cabin: Cabin): String = {
    val needsToInverseDirection = cabin.needsToInverseDirection()
    if (needsToInverseDirection) {
      Logger.debug("At top or bottom floor => inverse the direction")
      val direction = if (needsToInverseDirection) cabin.direction.inverse else cabin.direction

      fromDirection(direction).to(cabin)
    }
    else if (cabin.canDoNothing()) {
      Logger.debug("Can do nothing because no stops & at the middle floor!")
      NothingCommand.to(cabin)
    }
    else {
      val bestDirection = findBestDirection(cabin)
      fromDirection(bestDirection).to(cabin)
    }
  }

  def findBestDirection(elevator: Cabin) = {
    if (elevator.isEmpty()) forceDirectionToMiddleFloor(elevator)
    else findBestDirectionByCurrentDirection(elevator)
  }

  def forceDirectionToMiddleFloor(elevator: Cabin): Direction = {
    Logger.debug("No stop, force replacement to the middle floor")
    if (elevator.floor < elevator.middleFloor) UP
    else DOWN
  }

  def findBestDirectionByCurrentDirection(elevator: Cabin): Direction = {
    val direction = elevator.direction
    Logger.debug(s"Look if has stop in current direction: $direction")

    elevator.getDirectionTypeForTravelers() match {
      case NextDirectionType.SAME_AS_CURRENT => direction
      case NextDirectionType.INVERSE => direction.inverse
      case _ => findDirectionByWaiters(elevator)
    }
  }

  def findDirectionByWaiters(elevator: Cabin): Direction = {
    elevator.getDirectionTypeForWaiters() match {
      case NextDirectionType.TO_UP => UP
      case NextDirectionType.TO_DOWN => DOWN
      case _ => forceDirectionToMiddleFloor(elevator)
    }
  }

}

class OpenCloseStrategy extends DirectionStrategy {

  override def nextCommand(elevator: Cabin): String = {
    if (elevator.canStop() && elevator.isDoorClosed) OpenCommand.to(elevator)
    else if (elevator.isDoorOpened) CloseCommand.to(elevator)
    else super.nextCommand(elevator)
  }

}
