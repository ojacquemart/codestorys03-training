package models

trait Stop {
  def fromFloor: Int
  def toFloor: Int
}

case class Call(fromFloor: Int, toFloor: Int, direction: Direction) extends Stop {

  override def hashCode(): Int = toFloor + direction.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: Call => toFloor == other.toFloor && direction == other.direction
    case _ => false
  }

  override def toString: String = s"Call(to=$toFloor,direction=$direction)"
}

case class Go(fromFloor: Int, toFloor: Int) extends Stop {

  override def hashCode(): Int = toFloor + toFloor.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: Go => toFloor == other.toFloor
    case _ => false
  }

  def currentDirection(currentFloor: Int) =  if (currentFloor >= toFloor) DOWN else UP
  override def toString: String = s"Stop(to=$toFloor)"
}