package models

import play.api.libs.json.Json
import play.api.Logger

case class Elevator(val lowerFloor: Int, val higherFloor: Int, val cabinSize: Int, val cabinCount: Int) {

  var hits = 0
  var points = 0

  val cabins = (0 until cabinCount).toList.map {
    i => new Cabin(i, lowerFloor, higherFloor, cabinSize)
  }

  // visible for test
  def callAndGo(cabin: Int, atFloor: Int, toFloor: Int, direction: Direction = UP) = {
    go(cabin, toFloor)
  }

  def call(atFloor: Int, direction: Direction) {
    val organizer = WaitersOrganizer(this)
    organizer.singleOrganize(atFloor, direction)
  }


  def userHasEntered(cabinIndex: Int): Unit = {
    val cabin = cabins(cabinIndex)
    cabin.removeHeadWaiter()
  }

  def removeWaiter(cabin: Cabin, maybeWaiter: Option[User]) = {
    if (maybeWaiter.isDefined) cabin.waiters.users -= maybeWaiter.get
  }

  def go(cabinIndex: Int, toFloor: Int) {
    val cabin = cabins(cabinIndex)
    cabin.travelers.addTraveler(cabin.floor, toFloor)
  }

  def userHasExited {}

  def beforeNextCommands() = {
    hits += 1
    cabins.foreach(_.beforeNextCommand())
  }
  
  def nextCommands(): String = {
    beforeNextCommands()
    val nextCommands = cabins.map(c => c.nextCommand()).mkString("\n")
    afterNextCommands()

    nextCommands
  }

  def afterNextCommands() = {
    WaitersOrganizer(this).organize()
  }

  def getStatus: String = Json.toJson(ElevatorRecap.get(this)).toString

}

object Elevator {

  def empty() = new Elevator(0, 0, 0, 0)
}

class WaitersOrganizer(val cabins: List[Cabin]) {

  def organize(): Unit = {
    val allWaiters = cabins.flatMap(_.waiters.users)
    Logger.debug(s"Let's tidy ${allWaiters.size} waiters")

    cabins.foreach(_.waiters.reset())
    val targetableCabins = getTargetableCabins()
    allWaiters.foreach(w => {
      organize(targetableCabins, w.fromFloor, w.direction)
    })
  }

  def singleOrganize(floor: Int, to: Direction): Unit = {
    organize(getTargetableCabins(), floor, to)
  }

  def organize(cabins: List[Cabin], floor: Int, to: Direction): Unit = {
    val nearestCabin = cabins.minBy(c => Math.abs(c.floor - floor))
    nearestCabin.waiters.addWaiter(floor, to)
  }

  def getTargetableCabins(): List[Cabin] = {
    val (emptyCabins, nonEmptyCabins) = cabins.partition(_.travelers.isEmpty)
    if (emptyCabins.size > 0) emptyCabins
    else nonEmptyCabins
  }

}

object WaitersOrganizer {

  def apply(elevator: Elevator) = {
    new WaitersOrganizer(elevator.cabins)
  }
}