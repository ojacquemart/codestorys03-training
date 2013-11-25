package models


import scala.collection.mutable.MutableList
import play.api.libs.json.Json

case class Elevator(val lowerFloor: Int, val higherFloor: Int, val cabinSize: Int, val cabinCount: Int) {

  var hits = 0
  var points = 0

  val cabins = (0 until cabinCount).toList.map {
    i => new Cabin(i, lowerFloor, higherFloor, cabinSize)
  }

  var nextFloors = MutableList[NextFloor]()

  // visible for test
  def callAndGo(atFloor: Int, toFloor: Int, cabin: Int, direction: Direction = UP) = {
    call(atFloor, direction)
    go(toFloor, cabin)
  }

  def call(atFloor: Int, direction: Direction) {
    cabins.foreach(c => c.users.add(atFloor, direction))
  }

  def userHasEntered {}

  def go(toFloor: Int, cabin: Int) {
    nextFloors += NextFloor(toFloor, cabin)
  }

  def onUserExited {}

  def nextCommands(): String = {
    hits += 1
    cabins.foreach(c => c.beforeNextCommand(nextFloors))
    nextFloors = MutableList()

    cabins.map(e => e.nextCommand()).mkString("\n")
  }

  def getStatus: String = Json.toJson(ElevatorRecap.get(this)).toString

}


