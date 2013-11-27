package models

import play.api.libs.json.Json

case class Elevator(val lowerFloor: Int, val higherFloor: Int, val cabinSize: Int, val cabinCount: Int) {

  var hits = 0
  var points = 0

  val cabins = (0 until cabinCount).toList.map {
    i => new Cabin(i, lowerFloor, higherFloor, cabinSize)
  }

  // visible for test
  def callAndGo(atFloor: Int, toFloor: Int, cabin: Int, direction: Direction = UP) = {
    call(atFloor, direction)
    go(toFloor, cabin)
  }

  def call(atFloor: Int, direction: Direction) {
    val (emptyCabins, nonEmptyCabins) = cabins.partition(_.users.isEmpty)
    if (emptyCabins.size > 0) nearestCabin(emptyCabins, atFloor, direction)
    else nearestCabin(nonEmptyCabins, atFloor, direction)
  }

  def nearestCabin(cabins: List[Cabin], floor: Int, to: Direction) = {
    val nearestCabin = cabins.minBy(c => Math.abs(c.floor - floor))
    nearestCabin.users.add(floor, to)
  }

  def userHasEntered {}

  def go(cabin: Int, toFloor: Int) {
    cabins.foreach(_.flagNextFloor(NextFloor(toFloor, cabin)))
  }

  def onUserExited {}

  def nextCommands(): String = {
    hits += 1
    cabins.foreach(_.onNextCommand())

    cabins.map(e => e.nextCommand()).mkString("\n")
  }

  def getStatus: String = Json.toJson(ElevatorRecap.get(this)).toString

}

object Elevator {

  def empty() = new Elevator(0, 0, 0, 0)
}


