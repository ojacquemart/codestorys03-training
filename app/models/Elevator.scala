package models

import play.api.Logger

trait DefaultElevator {

  var floor: Int = 0
  val maxFloor: Int
  val middleFloor = maxFloor / 2

  var direction: Direction = UP
  var opened: Boolean = false

  def needsToInverseDirection(): Boolean = (direction == UP && isAtTop) || (direction == DOWN && isAtBottom)

  def isAtTop: Boolean = floor == maxFloor - 1
  def isAtBottom: Boolean = floor == 0
  def isAtMiddle: Boolean = floor == middleFloor

  def reset(lowerFloor: Int) = {
    floor = lowerFloor
    direction = UP
    opened = false
  }
}

case class SimpleElevator(maxFloor: Int, strategy: Strategy) extends DefaultElevator {

  def getNextCommand() = strategy.getNextCommand(this)

  def getStops() = strategy.stops

  def gotTo(toFloor: Int) = {
    strategy.addStop(new Stop(floor, toFloor, upOrDown(floor, toFloor)))
  }

  private def upOrDown(current: Int, to: Int) = if (current >= to) DOWN else UP

  override def reset(lowerFloor: Int): Unit = {
    super.reset(lowerFloor)
    strategy.reset
  }

  def getStatus: String = s"floor=$floor, open=$opened, direction=$direction, stops=${getStops()}"
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

  var stops: Set[Stop] = Set()

  def getNextCommand(elevator: DefaultElevator): String

  def addStop(stop: Stop) = stops += stop

  def reset = {
    stops = Set()
  }

}

class UpAndDownStrategy extends Strategy {

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

  def canDoNothing(elevator: DefaultElevator) = elevator.isAtMiddle && stops.isEmpty

  def findBestDirection(elevator: DefaultElevator) = {
    if (stops.isEmpty) forceDirectionToMiddleFloor(elevator)
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

    val stopsInCurrentDirection = stops.count(stop => stop.direction == elevator.direction)
    val keepsCurrentDirection = stopsInCurrentDirection > 0
    Logger.debug(s"\tNo stop in direction $direction => keeps direction = $keepsCurrentDirection")

    if (keepsCurrentDirection) direction
    else direction.inverse
  }

}

class WithStopStrategy extends UpAndDownStrategy {

  override def getNextCommand(elevator: DefaultElevator): String = {
    Logger.debug(s"Current stops: $stops")
    val maybeStop = stops.find(stop => stop.to == elevator.floor)

    if (maybeStop.isDefined) {
      Logger.debug(s"Remove stop ${maybeStop.get}")
      stops -= maybeStop.get
      if (!elevator.opened) {
        return OpenCommand.to(elevator)
      }
    }
    if (elevator.opened) return CloseCommand.to(elevator)

    return super.getNextCommand(elevator)
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