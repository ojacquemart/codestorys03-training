package models

import play.api.Logger

trait Elevator {

  var floor: Int = 0
  val maxFloor: Int
  val middleFloor = maxFloor / 2

  var direction: Direction = UP
  var opened: Boolean = false

  var users = 0

  def needsToInverseDirection(): Boolean = (direction == UP && isAtTop) || (direction == DOWN && isAtBottom)

  def isAtTop: Boolean = floor == maxFloor - 1
  def isAtBottom: Boolean = floor == 0
  def isAtMiddle: Boolean = floor == middleFloor

  def addUser = users += 1
  def removeUser = users -= 1

  def reset(lowerFloor: Int = 0) = {
    floor = lowerFloor
    users = 0
    direction = UP
    opened = false
  }
}

case class SimpleElevator(maxFloor: Int, strategy: Strategy) extends Elevator {

  def getNextCommand() = strategy.getNextCommand(this)

  def getGos() = strategy.gos

  def getCalls() = strategy.calls

  def call(atFloor: Int, direction: Direction) {
    strategy.addCall(atFloor, direction)
  }

  def go(toFloor: Int) = {
    strategy.addGo(toFloor)
  }

  def upOrDown(current: Int, to: Int) = if (current >= to) DOWN else UP

  override def reset(lowerFloor: Int): Unit = {
    super.reset(lowerFloor)
    strategy.reset
  }

  def getStatus: String = s"floor=$floor, open=$opened, users=$users, " +
    s"direction=$direction, calls=${getCalls()}}, gos=${getGos()}"
}

trait Strategy {

  var calls: Set[Call] = Set()
  var gos: Set[Go] = Set()

  def addCall(atFloor: Int, direction: Direction) = {
    calls += new Call(atFloor, direction)
  }

  def addGo(toFloor: Int) = {
    gos += new Go(toFloor)
  }

  def getNextCommand(elevator: Elevator): String

  def addGo(go: Go) = gos += go
  def addCall(call: Call) = calls += call
  def canDoNothing(elevator: Elevator) = elevator.isAtMiddle && hasNoCallAndGo()
  def hasNoCallAndGo() = gos.isEmpty && calls.isEmpty

  def hasNoGo() = gos.isEmpty
  def hasOneCall() = calls.size == 1
  def firstCallDirection() = calls.head.direction

  def reset = {
    gos = Set()
    calls = Set()
  }

}

class DirectionStrategy extends Strategy {

  def fromDirection(direction: Direction): Command = {
    if (direction == UP) UpCommand
    else DownCommand
  }

  def getNextCommand(elevator: Elevator): String = {
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

    val firstCallFloor = calls.toList.sortBy(_.toFloor).head.toFloor
    Logger.debug(s"Lets go the first call: $firstCallFloor")
    val goesUp = needsToGoUp(elevator.floor, firstCallFloor)
    Logger.debug(s"\tNeeds to go up: $goesUp")

    if (goesUp) UP else DOWN
  }

  def needsToGoUp(currentFloor: Int, toFloor: Int) = currentFloor < toFloor

}

class OpenCloseStrategy extends DirectionStrategy {

  override def getNextCommand(elevator: Elevator): String = {
    Logger.debug(s"Current calls=$calls, gos=$gos")

    if (needsStop(elevator)) {
      if (!elevator.opened) {
        return OpenCommand.to(elevator)
      }
    }
    if (elevator.opened) return CloseCommand.to(elevator)

    return super.getNextCommand(elevator)
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

