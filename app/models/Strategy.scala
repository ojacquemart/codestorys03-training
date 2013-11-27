package models

import play.api.Logger

trait Strategy {
  def nextCommand(cabin: Cabin): String
}

class OpenCloseStrategy extends DirectionStrategy {

  override def nextCommand(elevator: Cabin): String = {
    if (elevator.canStop() && elevator.isDoorClosed) OpenCommand.to(elevator)
    else if (elevator.isDoorOpened) CloseCommand.to(elevator)
    else super.nextCommand(elevator)
  }

}

class DirectionStrategy extends Strategy {

  def fromDirection(direction: Direction): Command = {
    if (direction == UP) UpCommand
    else DownCommand
  }

  def nextCommand(cabin: Cabin): String = {
    val needsToInverseDirection = cabin.needsToInverseDirection()
    if (needsToInverseDirection) {
      Logger.debug(s"CABIN ${cabin.index} - At top or bottom floor => inverse the direction")
      val direction = if (needsToInverseDirection) cabin.direction.inverse else cabin.direction

      fromDirection(direction).to(cabin)
    }
    else if (cabin.canDoNothing()) {
      Logger.debug(s"CABIN ${cabin.index} - Can do nothing because no stops & at the middle floor!")
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

  def forceDirectionToMiddleFloor(cabin: Cabin): Direction = {
    Logger.debug(s"CABIN ${cabin.index} - No stop, force replacement to the middle floor")
    if (cabin.floor < cabin.middleFloor) UP
    else DOWN
  }

  def findBestDirectionByCurrentDirection(cabin: Cabin): Direction = {
    val direction = cabin.direction
    Logger.debug(s"CABIN ${cabin.index} - Look if has stop in current direction: $direction")

    cabin.getDirectionTypeForTravelers() match {
      case NextDirectionType.SAME_AS_CURRENT => direction
      case NextDirectionType.INVERSE => direction.inverse
      case _ => findDirectionByWaiters(cabin)
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