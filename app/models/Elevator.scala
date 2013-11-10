package models

import play.api.Logger

import scala.collection.mutable.Map

trait Reset {
  def reset(): Unit
}

trait Elevator extends Reset {

  val maxFloor: Int
  val middleFloor = maxFloor / 2

  var floor: Int = 0

  var direction: Direction = UP
  var door = Door.CLOSE

  var users = 0
  var waiters = new Waiters

  def needsToInverseDirection(): Boolean = (direction == UP && isAtTop) || (direction == DOWN && isAtBottom)

  def isAtTop: Boolean = floor == maxFloor - 1
  def isAtBottom: Boolean = floor == 0
  def isAtMiddle: Boolean = floor == middleFloor

  def call(atFloor: Int, direction: Direction) {
    waiters.addAt(atFloor)
  }

  def go(toFloor: Int) = { }

  def userHasEntered = {
    users += 1
    waiters.removeAt(floor)
  }

  def onUserExited = users -= 1

  def resetToFloor(lowerFloor: Int = 0) = {
    floor = lowerFloor
    reset()
  }

  override def reset() = {
    Logger.debug("Elevator reset")
    users = 0
    direction = UP
    door = Door.CLOSE
    waiters.reset
  }
}

case class SimpleElevator(maxFloor: Int, strategy: Strategy) extends Elevator with Reset {

  def nextCommand() = strategy.nextCommand(this)

  override def call(atFloor: Int, direction: Direction) {
    super.call(atFloor, direction)
    strategy.addCall(floor, atFloor, direction)
  }

  override def go(toFloor: Int) = {
    strategy.addGo(floor, toFloor)
  }

  override def reset() = {
    super.reset()
    strategy.reset
  }

  def getStatus: String = s"""
      floor=$floor
      users=$users
      door=$door
      direction=$direction
      waiters=${debugWaiters()}
      calls=${debugCalls()}
      gos=${debugGos()}"""

  def debugGos() = strategy.gos.toList.sortBy(_.toFloor).mkString(",")
  def debugCalls() = strategy.calls.toList.sortBy(_.toFloor).mkString(",")
  def debugWaiters() = waiters.filterByPositive()
    .map(waiter => s"Floor(${waiter._1}, ${waiter._2})")
    .mkString(",")
  
}

class Waiters extends Reset {

  var waitersByFloor = emptyWaiters

  def filterByPositive() = waitersByFloor.filter(_._2 > 0)
    .toList
    .sortBy(_._1)

  def addAt(floor: Int) {
    waitersByFloor(floor) += 1
  }

  def removeAt(floor: Int) {
    if (countAt(floor) > 0) waitersByFloor(floor) -= 1
  }

  def countAt(floor: Int) = waitersByFloor(floor)

  def reset() {
    waitersByFloor = emptyWaiters
  }

  def emptyWaiters = Map[Int, Int]().withDefaultValue(0)
}

trait Strategy {

  var calls: Set[Call] = Set()
  var gos: Set[Go] = Set()

  def addCall(fromFloor: Int, atFloor: Int, direction: Direction) = {
    calls += new Call(fromFloor, atFloor, direction)
  }

  def addGo(fromFloor: Int, toFloor: Int) = {
    gos += new Go(fromFloor, toFloor)
  }

  def nextCommand(elevator: Elevator): String

  def canDoNothing(elevator: Elevator) = elevator.isAtMiddle && hasNoCallAndGo()
  def hasNoCallAndGo() = gos.isEmpty && calls.isEmpty

  def reset = {
    Logger.debug("Reset calls and gos")
    calls = Set()
    gos = Set()
  }

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
    else if (canDoNothing(elevator)) {
      Logger.debug("Can do nothing because no stops & at the middle floor!")
      NothingCommand.to(elevator)
    }
    else {
      val bestDirection = findBestDirection(elevator)
      fromDirection(bestDirection).to(elevator)
    }
  }


  def findBestDirection(elevator: Elevator) = {
    if (hasNoCallAndGo()) forceDirectionToMiddleFloor(elevator)
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

    val (gosSameDirection, gosOtherDirection) = gos.partition(go => go.currentDirection(elevator.floor) == elevator.direction)
    if (gosSameDirection.size > 0) {
      Logger.debug(s"Stops in current direction: continue ${direction}")
      return direction
    }
    if (gosOtherDirection.size > 0) {
      Logger.debug(s"No stops in current direction: inverse direction to ${direction.inverse}")
      return direction.inverse
    }

    return findDirectionByCalls(elevator)
  }

  def findDirectionByCalls(elevator: Elevator): Direction = {
    if (calls.isEmpty) {
      Logger.debug("No call, force direction to middle floor")
      return forceDirectionToMiddleFloor(elevator)
    }

    val firstCallFloor = nearestFloorByCurrentFloor(elevator.floor)
    Logger.debug(s"Lets go the first call: $firstCallFloor")
    val goesUp = needsToGoUp(elevator.floor, firstCallFloor)
    Logger.debug(s"\tNeeds to go up: $goesUp")

    if (goesUp) UP else DOWN
  }

  def nearestFloorByCurrentFloor(currentFloor: Int): Int = {
    calls.toList
      .sortBy(_.toFloor)
      .minBy(call => Math.abs(currentFloor - call.toFloor))
      .toFloor
  }

  def needsToGoUp(currentFloor: Int, toFloor: Int) = currentFloor < toFloor

}

class OpenCloseStrategy extends DirectionStrategy {

  override def nextCommand(elevator: Elevator): String = {
    Logger.debug(s"Current calls=$calls, gos=$gos")

    if (needsStop(elevator)) {
      if (elevator.door == Door.CLOSE) {
        return OpenCommand.to(elevator)
      }
    }
    if (elevator.door == Door.OPEN) return CloseCommand.to(elevator)

    gos.foreach(_.ticks += 1)

    return super.nextCommand(elevator)
  }

  def needsStop(elevator: Elevator): Boolean = {
    // Lists of Option[SimpleStop] to check stops.
    val neededStops = List(getStopFromFloor(elevator.floor),
      getCallFromFloorFloorInCurrentDirection(elevator))
    val needsStop = neededStops.count(_.size == 1) > 0
    Logger.debug(s"Needs stop = $needsStop for $neededStops from floor ${elevator.floor} to ${elevator.direction}")

    if (needsStop) {
      neededStops.foreach(maybeStop => maybeStop match {
        case Some(go: Go) => gos -= go
        case Some(call: Call) => calls = calls.filterNot(_.toFloor == call.toFloor)
        case _ => {}
      })
    }

    needsStop
  }

  def getStopFromFloor(floor: Int): Option[Go] = gos.find(go => go.toFloor == floor)

  def getCallFromFloorFloorInCurrentDirection(elevator: Elevator) = {
    // No gos, check if has one call whatever the direction
    if (gos.isEmpty) calls.find(_.toFloor == elevator.floor)
    else calls.find(c => c.toFloor == elevator.floor && c.direction == elevator.direction)
  }
}

