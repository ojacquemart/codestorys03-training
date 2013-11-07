package models

import play.api.Logger

trait DefaultElevator {

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

case class SimpleElevator(maxFloor: Int, strategy: Strategy) extends DefaultElevator {

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

trait SimpleStop {
  def toFloor: Int
}

case class Call(toFloor: Int, direction: Direction) extends SimpleStop
case class Go(toFloor: Int) extends SimpleStop {

  def currentDirection(currentFloor: Int) =  if (currentFloor >= toFloor) DOWN else UP
}

case class Stop(from: Int, to: Int, direction: Direction ) {

  override def hashCode(): Int = to

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: Stop => to == other.to
    case _ => false
  }

  override def toString = s"[floor=$to,to=$direction]"
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

  def getNextCommand(elevator: DefaultElevator): String

  def addGo(go: Go) = gos += go
  def addCall(call: Call) = calls += call
  def canDoNothing(elevator: DefaultElevator) = elevator.isAtMiddle && hasNoCallAndGo()
  def hasNoCallAndGo() = gos.isEmpty && calls.isEmpty

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

  def getNextCommand(elevator: DefaultElevator): String = {
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


  def findBestDirection(elevator: DefaultElevator) = {
    if (hasNoCallAndGo()) forceDirectionToMiddleFloor(elevator)
    else findBestDirectionByCurrentDirection(elevator)

  }

  def forceDirectionToMiddleFloor(elevator: DefaultElevator): Direction = {
    Logger.debug("No stop, force replacement to the middle floor")
    if (elevator.floor < elevator.middleFloor) UP
    else DOWN
  }

  def findBestDirectionByCurrentDirection(elevator: DefaultElevator): Direction = {
    val direction = elevator.direction
    Logger.debug(s"Look if has stop in current direction: $direction")

    val gosInCurrentDirection = gos.count(go => go.currentDirection(elevator.floor) == elevator.direction)
    if (gosInCurrentDirection > 0) {
      return direction
    }

    // Prefers deserve people in cabins than callers waiting outside.
    if (!gos.isEmpty) {
      Logger.debug(s"No gos, inverse direction to ${direction.inverse}")
      return direction.inverse
    }

    // No calls, and already treat gos, return to middle.
    if (calls.isEmpty) {
      Logger.debug("No call, force direction to middle floor")
      return forceDirectionToMiddleFloor(elevator)
    }

    // TODO: improve search of call.
    val callsInCurrentDirection = calls.filter(call => call.direction == elevator.direction)
    Logger.debug("Search for min call and best direction")
    if (!callsInCurrentDirection.isEmpty) {
      val nearestCallInCurrentDirection = callsInCurrentDirection.toList.minBy(call => Math.min(elevator.floor, call.toFloor))
      return directionComparingFloors(nearestCallInCurrentDirection.toFloor, elevator.floor)
    }

    val nearestCallByFloor = calls.minBy(call => Math.min(elevator.floor, call.toFloor))
    return directionComparingFloors(nearestCallByFloor.toFloor, elevator.floor)
  }
  
  def directionComparingFloors(a: Int, b: Int) = {
    if (a < b) DOWN else UP
  }

}

class OpenCloseStrategy extends DirectionStrategy {

  override def getNextCommand(elevator: DefaultElevator): String = {
    Logger.debug(s"Current calls=$calls, gos=$gos")

    if (needsStop(elevator)) {
      if (!elevator.opened) {
        return OpenCommand.to(elevator)
      }
    }
    if (elevator.opened) return CloseCommand.to(elevator)

    return super.getNextCommand(elevator)
  }

  def needsStop(elevator: DefaultElevator): Boolean = {
    val neededStops = List(getStopFromFloor(elevator.floor),
        getCallFromFloorFloorInCurrentDirection(elevator),
        getCallWhenNoGos(elevator.floor))
    val needsStop = neededStops.count(_.size == 1) > 0
    Logger.debug(s"Needs stop = $needsStop for $neededStops from floor ${elevator.floor} to ${elevator.direction}")
    
    if (needsStop) {
      neededStops.foreach(maybeStop => maybeStop match {
        case Some(go: Go) => gos -= go
        case Some(call: Call) => calls -= call
        case _ => {}
      })
    }

    needsStop
  }

  def getStopFromFloor(floor: Int): Option[Go] = gos.find(go => go.toFloor == floor)

  def getCallFromFloorFloorInCurrentDirection(elevator: DefaultElevator) =
    calls.find(c => c.toFloor == elevator.floor && c.direction == elevator.direction)

  def getCallWhenNoGos(floor: Int) = {
    if (gos.isEmpty) calls.find(_.toFloor == floor)
    else None
  }
}

trait Command {
  def to(elevator: DefaultElevator): String
}

object NothingCommand extends Command {
  def to(elevator: DefaultElevator): String = "NOTHING"
}

object UpCommand extends Command {
  def to(elevator: DefaultElevator): String = {
    if (elevator.isAtTop) NothingCommand.to(elevator)
    else {
      elevator.floor += 1
      elevator.direction = UP
      return "UP"
    }
  }
}

object DownCommand extends Command {
  def to(elevator: DefaultElevator): String = {
    if (elevator.isAtBottom) NothingCommand.to(elevator)
    else {
      elevator.floor -= 1
      elevator.direction = DOWN
      "DOWN"
    }
  }
}

object OpenCommand extends Command {
  def to(elevator: DefaultElevator): String = {
    if (!elevator.opened) {
      elevator.opened = true
      "OPEN"
    }
    else NothingCommand.to(elevator)
  }
}

object CloseCommand extends Command {
  def to(elevator: DefaultElevator): String = {
    if (elevator.opened) {
      elevator.opened = false
      "CLOSE"
    }
    else NothingCommand.to(elevator)
  }
}

trait Direction {
  def name: String

  def inverse: Direction = if (name == "UP") DOWN else UP

  override def toString: String = name
}

object UP extends Direction {
  def name = "UP"
}

object DOWN extends Direction {
  def name = "DOWN"
}

object Directions {
  def valueOf(direction: String) = if (direction == "UP") UP else DOWN
}