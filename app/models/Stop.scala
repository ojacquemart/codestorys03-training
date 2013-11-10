package models

trait Stop {
  def fromFloor: Int
  def toFloor: Int
  def ticks: Int
}

case class Call(fromFloor: Int, toFloor: Int, direction: Direction, ticks: Int = 0) extends Stop {

  override def hashCode(): Int = toFloor + direction.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: Call => toFloor == other.toFloor && direction == other.direction
    case _ => false
  }

  override def toString: String = s"Call(from=$fromFloor, to=$toFloor)"
}
case class Go(fromFloor: Int, toFloor: Int, var ticks: Int = 0) extends Stop {


  override def hashCode(): Int = toFloor + toFloor.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: Go => toFloor == other.toFloor
    case _ => false
  }

  def currentDirection(currentFloor: Int) =  if (currentFloor >= toFloor) DOWN else UP
  override def toString: String = s"Stop(from=$fromFloor, to=$toFloor, ticks=$ticks)"
}