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
  cabinsStatus: List[CabinStatus])

case class CabinStatus(
   index: Int,
   floor: Int,
   cabinSize: Int,
   door: String,
   direction: String,
   lowerFloor: Int,
   higherFloor: Int,
   middleFloor: Int,
   usersStatus: UsersStatus)

object CabinStatus {

  def get(cabin: Cabin) = {
    new CabinStatus(
      cabin.index,
      cabin.floor,
      cabin.size,
      cabin.door.toString,
      cabin.direction.toString,
      cabin.lowerFloor, cabin.higherFloor,
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
      cabin.users.size,
      cabin.users.waitersSize,
      cabin.users.travelersSize,
      cabin.users.waitersByFloor,
      cabin.users.travelersByFloor
    )
  }
}

object ElevatorStatus {

  import ElevatorRecap._
  implicit val writer = Json.writes[ElevatorStatus]

  def get(elevator: Elevator) = {
    val cabinsStatus = elevator.cabins.map(c => CabinStatus.get(c))
    new ElevatorStatus(elevator.hits, cabinsStatus)
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
