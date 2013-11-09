package models

trait Stop {
  def toFloor: Int
}

case class Call(toFloor: Int, direction: Direction) extends Stop
case class Go(toFloor: Int) extends Stop {

  def currentDirection(currentFloor: Int) =  if (currentFloor >= toFloor) DOWN else UP
}