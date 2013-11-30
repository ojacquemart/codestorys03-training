package models

import org.joda.time.DateTime
import play.api.libs.json.{JsValue, Writes, Json}
import org.joda.time.format.DateTimeFormat

case class ElevatorRecap(status: ElevatorStatus)

object ElevatorRecap {

  implicit val usersStatusWriter = Json.writes[UsersStatus]
  implicit val singleCabinWriter = Json.writes[CabinStatus]
  implicit val elevatorStatusWriter = Json.writes[ElevatorStatus]
  implicit val writer = Json.writes[ElevatorRecap]

  def get(elevator: Elevator) = {
    new ElevatorRecap(
      ElevatorStatus.get(elevator)
    )
  }
}

case class ElevatorStatus(
  hits: Int,
  lowerFloor: Int,
  higherFloor: Int,
  cabinSize: Int,
  cabinCount: Int,
  cabinsStatus: List[CabinStatus])

object ElevatorStatus {

  import ElevatorRecap._
  implicit val writer = Json.writes[ElevatorStatus]

  def get(elevator: Elevator) = {
    val cabinsStatus = elevator.cabins.map(c => CabinStatus.get(c))
    new ElevatorStatus(
      elevator.hits,
      elevator.lowerFloor,
      elevator.higherFloor,
      elevator.cabinSize,
      elevator.cabinCount,
      cabinsStatus)
  }

}

case class CabinStatus(
   index: Int,
   floor: Int,
   door: String,
   direction: String,
   middleFloor: Int,
   usersStatus: UsersStatus)

object CabinStatus {

  def get(cabin: Cabin) = {
    new CabinStatus(
      cabin.index,
      cabin.floor,
      cabin.door.toString,
      cabin.direction.toString,
      cabin.middleFloor,
      UsersStatus.get(cabin))
  }
}

case class UsersStatus(
  total: Int,
  waiters: Int,
  travelers: Int,
  waitersByFloor: List[UserByFloor],
  travelersByFloor: List[UserByFloor])

object UsersStatus {
  def get(cabin: Cabin) = {
    new UsersStatus(
      cabin.size,
      cabin.waiters.size,
      cabin.travelers.size,
      cabin.waitersByFloor,
      cabin.travelersByFloor
    )
  }
}

case class SimpleReset(
  cause: String,
  status: ElevatorStatus,
  time: DateTime = DateTime.now())

object SimpleReset {

  val dtf = DateTimeFormat.forPattern("dd/MM/yy HH:mm:ss")

  implicit object Writer extends Writes[SimpleReset] {
    def writes(o: SimpleReset): JsValue = {
      Json.obj(
        "cause" -> o.cause,
        "time" -> dtf.print(o.time),
        "status" -> o.status)
    }
  }
}
