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
    val (emptyCabins, nonEmptyCabins) = cabins.partition(_.travelers.isEmpty)
    if (emptyCabins.size > 0) addWaiterToNearestCabin(emptyCabins, atFloor, direction)
    else addWaiterToNearestCabin(nonEmptyCabins, atFloor, direction)
  }

  def addWaiterToNearestCabin(cabins: List[Cabin], floor: Int, to: Direction) = {
    val nearestCabin = cabins.minBy(c => Math.abs(c.floor - floor))
    nearestCabin.waiters.addWaiter(floor, to)
  }

  def userHasEntered(cabinIndex: Int) = {
    val cabin = cabins(cabinIndex)
    val maybeWaiter = cabin.waiters.users.find(w => w.fromFloor == cabin.floor)
    if (maybeWaiter.isDefined) cabin.waiters.users -= maybeWaiter.get
  }

  def go(cabinIndex: Int, toFloor: Int) {
    val cabin = cabins(cabinIndex)
    cabin.travelers.addTraveler(cabin.floor, toFloor)
  }

  def onUserExited {}

  def nextCommands(): String = {
    hits += 1
    cabins.foreach(_.onNextCommand())

    cabins.map(c => c.nextCommand()).mkString("\n")
  }

  def getStatus: String = Json.toJson(ElevatorRecap.get(this)).toString

}

object Elevator {

  def empty() = new Elevator(0, 0, 0, 0)
}
