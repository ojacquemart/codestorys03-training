package models

trait Command {
  def to(elevator: Elevator): String
}

object NothingCommand extends Command {
  def to(elevator: Elevator): String = "NOTHING"
}

object UpCommand extends Command {
  def to(elevator: Elevator): String = {
    if (elevator.isAtTop) NothingCommand.to(elevator)
    else {
      elevator.floor += 1
      elevator.direction = UP
      return "UP"
    }
  }
}

object DownCommand extends Command {
  def to(elevator: Elevator): String = {
    if (elevator.isAtBottom) NothingCommand.to(elevator)
    else {
      elevator.floor -= 1
      elevator.direction = DOWN
      "DOWN"
    }
  }
}

object OpenCommand extends Command {
  def to(elevator: Elevator): String = {
    if (elevator.door == Door.CLOSE) {
      elevator.door = Door.OPEN
      "OPEN"
    }
    else NothingCommand.to(elevator)
  }
}

object CloseCommand extends Command {
  def to(elevator: Elevator): String = {
    if (elevator.door == Door.OPEN) {
      elevator.door = Door.CLOSE
      "CLOSE"
    }
    else NothingCommand.to(elevator)
  }
}