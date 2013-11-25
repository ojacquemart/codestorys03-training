package models

trait Command {
  def to(cabin: Cabin): String
}

object NothingCommand extends Command {
  def to(cabin: Cabin): String = "NOTHING"
}

object UpCommand extends Command {
  def to(cabin: Cabin): String = {
    if (cabin.isAtTop) NothingCommand.to(cabin)
    else {
      cabin.floor += 1
      cabin.direction = UP
      return "UP"
    }
  }
}

object DownCommand extends Command {
  def to(cabin: Cabin): String = {
    if (cabin.isAtBottom) NothingCommand.to(cabin)
    else {
      cabin.floor -= 1
      cabin.direction = DOWN
      "DOWN"
    }
  }
}

object OpenCommand extends Command {
  def to(cabin: Cabin): String = {
    if (cabin.door == Door.CLOSE) {
      cabin.door = Door.OPEN
      "OPEN"
    }
    else NothingCommand.to(cabin)
  }
}

object CloseCommand extends Command {
  def to(cabin: Cabin): String = {
    if (cabin.door == Door.OPEN) {
      cabin.door = Door.CLOSE
      "CLOSE"
    }
    else NothingCommand.to(cabin)
  }
}