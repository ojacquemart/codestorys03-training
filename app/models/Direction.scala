package models

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