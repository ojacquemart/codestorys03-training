package models

import play.api.Logger

import scala.collection.mutable.MutableList
import play.api.libs.json.Json

trait Elevator extends Reset {
  var hits = 0
  var points = 0

  var cabinSize: Int

  var floor = 0
  var lowerFloor = 0
  var higherFloor: Int
  def middleFloor = (higherFloor / 2) + 1

  var direction: Direction = UP
  var door = Door.CLOSE

  var users = new Users(cabinSize)

  var nextFloorsToGo = MutableList[Int]()

  var resets = MutableList[SimpleReset]()

  def needsToInverseDirection(): Boolean = (direction == UP && isAtTop) || (direction == DOWN && isAtBottom)

  def isAtTop: Boolean = floor == higherFloor
  def isAtBottom: Boolean = floor == lowerFloor
  def isAtMiddle: Boolean = floor == middleFloor

  def isDoorOpened = door == Door.OPEN
  def isDoorClosed = door == Door.CLOSE

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

  def isCabinFullAt80Percents() = users.travelersSize > (cabinSize * 0.8).toInt

  // for testing
  def resetToFloor(lowerFloor: Int = 0, higherFloor: Int = 19, maxCabinSize: Int = 30) {
    reset(0, higherFloor, maxCabinSize)
    floor = lowerFloor
  }

  def reset(lowerFloor: Int = 0, higherFloor: Int = 19, maxCabinSize: Int = 30, cause: String = "") {
    if (resets.size > 50) resets = MutableList()

    resets += SimpleReset(cause, ElevatorStatus.get(this))

    floor = 0
    this.lowerFloor = lowerFloor
    this.higherFloor = higherFloor
    this.cabinSize = maxCabinSize
    reset()
  }

  def reset(): Unit = {
    Logger.debug("Elevator reset")
    points = 0
    direction = UP
    door = Door.CLOSE
    users.resetUsers(cabinSize)
  }
}

case class SimpleElevator(var higherFloor: Int, var cabinSize: Int, strategy: Strategy) extends Elevator {

  def nextCommand() = {
    hits += 1
    users.onNextCommand(floor, nextFloorsToGo)
    nextFloorsToGo = MutableList()

    strategy.nextCommand(this)
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

  def getStatus: String = Json.toJson(ElevatorRecap.get(this)).toString

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
    if (elevator.canStop() && elevator.isDoorClosed) OpenCommand.to(elevator)
    else if (elevator.isDoorOpened) CloseCommand.to(elevator)
    else super.nextCommand(elevator)
  }

}

