package models

trait Stop {
  def fromFloor: Int
  def toFloor: Int
}

case class Call(fromFloor: Int, toFloor: Int, direction: Direction) extends Stop
case class Go(fromFloor: Int, toFloor: Int) extends Stop {

  def currentDirection(currentFloor: Int) =  if (currentFloor >= toFloor) DOWN else UP
}