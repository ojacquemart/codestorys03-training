package models

import play.api.Logger

trait DefaultElevator {

  var floor: Int = 0
  val maxFloor: Int
  var direction: Direction = UP
  var opened: Boolean = false

  def needsToInverseDirection(): Boolean = (direction == UP && isAtTop) || (direction == DOWN && isAtBottom)

  def isAtTop: Boolean = floor == maxFloor - 1
  def isAtBottom: Boolean = floor == 0

  def reset = {
    floor = 0
    direction = UP
    opened = false
  }
}

case class SimpleElevator(maxFloor: Int, strategy: Strategy) extends DefaultElevator {

  def getNextCommand() = strategy.getNextCommand(this)

  def gotTo(toFloor: Int) = {
    strategy.addStop(new Stop(floor, toFloor, upOrDown(floor, toFloor)))
  }

  private def upOrDown(current: Int, to: Int) = if (current > to) DOWN else UP
}

case class Stop(fromFloor: Int, toFloor: Int, direction: Direction ) {
  Logger.info(s"new Stop(fom: $fromFloor, to: $toFloor, direction: $direction")

  override def hashCode(): Int = toFloor

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: Stop => toFloor == other.toFloor
    case _ => false
  }

  override def toString = toFloor.toString
}

trait Strategy {

  def getNextCommand(elevator: DefaultElevator): String

  def addStop(stop: Stop) = {}
}

class UpAndDownStrategy extends Strategy {

  def fromDirection(direction: Direction): Command = {
    if (direction == UP) UpCommand
    else DownCommand
  }

  def getNextCommand(elevator: DefaultElevator): String = {
    val direction = if (elevator.needsToInverseDirection()) elevator.direction.inverse else elevator.direction

    Logger.info(s"Current floor ${elevator.floor}")
    fromDirection(direction).to(elevator)
  }

}

class StopStrategy extends UpAndDownStrategy {

  var stops: Set[Stop] = Set()

  override def addStop(stop: Stop) = stops += stop

  override def getNextCommand(elevator: DefaultElevator): String = {
    Logger.debug(s"Stops = $stops")
    val maybeStop =  stops.find(stop => stop.toFloor == elevator.floor)

    if (maybeStop.isDefined) {
      Logger.info("Remove " + maybeStop.get)
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

}

object UP extends Direction {
  def name = "UP"
}

object DOWN extends Direction {
  def name = "DOWN"
}
