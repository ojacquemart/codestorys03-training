package models

import play.api.Logger

import scala.collection.mutable.MutableList

trait Elevator extends Reset {

  val maxFloor: Int
  val middleFloor = maxFloor / 2

  var floor: Int = 0
  var direction: Direction = UP
  var door = Door.CLOSE
  var points = 0

  val users = new Users

  // To store the next goTo and update theirs values to the users.
  var nextFloorsToGo = MutableList[Int]()

  def needsToInverseDirection(): Boolean = (direction == UP && isAtTop) || (direction == DOWN && isAtBottom)
  def isAtTop: Boolean = floor == maxFloor - 1
  def isAtBottom: Boolean = floor == 0

  def isAtMiddle: Boolean = floor == middleFloor

  // visible for test
  def callAndGo(atFloor: Int, toFloor: Int, direction: Direction = UP) = {
    call(atFloor, direction)
    go(toFloor)
  }

  def call(atFloor: Int, direction: Direction) {
    users.add(atFloor, direction)
  }

  def userHasEntered {}

  def go(toFloor: Int) {
    nextFloorsToGo += toFloor
  }
  def onUserExited {}

  def canDoNothing() = isAtMiddle && isEmpty()
  def canStop(): Boolean
  def getDirectionTypeForTravelers(): NextDirectionType.Value
  def getDirectionTypeForWaiters(): NextDirectionType.Value

  def isEmpty() = users.size == 0

  def resetToFloor(lowerFloor: Int = 0) {
    floor = lowerFloor
    reset()
  }

  def reset(): Unit = {
    Logger.debug("Elevator reset")
    direction = UP
    door = Door.CLOSE
    users.reset
  }
}

case class SimpleElevator(maxFloor: Int, strategy: Strategy) extends Elevator {

  def nextCommand() = {
    nextFloorsToGo = users.updateNextFloorsToGo(floor, nextFloorsToGo)
    users.tick()

    val command = strategy.nextCommand(this)
    users.removeDone()

    command
  }

  def canStop() = {
    val canStop = users.canStopAt(floor, direction)
    if (canStop) {
      Logger.debug("Stop travelers and update points")
      users.stopTravelersAt(floor)
      points += users.donerScores
    }
    canStop
  }

  def getDirectionTypeForTravelers(): NextDirectionType.Value = {
    users.getDirectionTypeForTravelers(floor, direction)
  }

  def getDirectionTypeForWaiters(): NextDirectionType.Value = {
    users.getDirectionTypeForWaiters(floor, direction)
  }

  def getStatus: String = s"""
      points=$points
      floor=$floor
      door=$door
      direction=$direction
      totalUsers=${users.size}
      nbWaiters=${users.waiters.size}
      nbTravelers=${users.travelers.size}
      waitersByFloor=${debugWaitersByFloor()}
      waiters=${debugWaiters()}
      travelers=${debugTravelers()}
      """

  def debugWaitersByFloor() = users.waiters
      .groupBy(_.fromFloor)
      .map(waitersByFloor => (waitersByFloor._1, waitersByFloor._2.size))
      .toList
      .sortBy(_._1)
      .map(w => s"${w._1} -> ${w._2}").mkString(", ")
  def debugTravelers() = users.travelers.sortBy(_.toFloor).mkString(",")
  def debugWaiters() = users.waiters.sortBy(_.toFloor).mkString(",")
  
}

trait Strategy {
  def nextCommand(elevator: Elevator): String
}

class DirectionStrategy extends Strategy {

  def fromDirection(direction: Direction): Command = {
    if (direction == UP) UpCommand
    else DownCommand
  }

  def nextCommand(elevator: Elevator): String = {
    val needsToInverseDirection = elevator.needsToInverseDirection()
    if (needsToInverseDirection) {
      Logger.debug("At top or bottom floor => inverse the direction")
      val direction = if (needsToInverseDirection) elevator.direction.inverse else elevator.direction

      fromDirection(direction).to(elevator)
    }
    else if (elevator.canDoNothing()) {
      Logger.debug("Can do nothing because no stops & at the middle floor!")
      NothingCommand.to(elevator)
    }
    else {
      val bestDirection = findBestDirection(elevator)
      fromDirection(bestDirection).to(elevator)
    }
  }

  def findBestDirection(elevator: Elevator) = {
    if (elevator.isEmpty()) forceDirectionToMiddleFloor(elevator)
    else findBestDirectionByCurrentDirection(elevator)
  }

  def forceDirectionToMiddleFloor(elevator: Elevator): Direction = {
    Logger.debug("No stop, force replacement to the middle floor")
    if (elevator.floor < elevator.middleFloor) UP
    else DOWN
  }

  def findBestDirectionByCurrentDirection(elevator: Elevator): Direction = {
    val direction = elevator.direction
    Logger.debug(s"Look if has stop in current direction: $direction")

    elevator.getDirectionTypeForTravelers() match {
      case NextDirectionType.SAME_AS_CURRENT => direction
      case NextDirectionType.INVERSE => direction.inverse
      case _ => findDirectionByWaiters(elevator)
    }
  }

  def findDirectionByWaiters(elevator: Elevator): Direction = {
    elevator.getDirectionTypeForWaiters() match {
      case NextDirectionType.TO_UP => UP
      case NextDirectionType.TO_DOWN => DOWN
      case _ => forceDirectionToMiddleFloor(elevator)
    }
  }

}

class OpenCloseStrategy extends DirectionStrategy {

  override def nextCommand(elevator: Elevator): String = {
    if (elevator.canStop() && elevator.door == Door.CLOSE) OpenCommand.to(elevator)
    else if (elevator.door == Door.OPEN) CloseCommand.to(elevator)
    else super.nextCommand(elevator)
  }
}

