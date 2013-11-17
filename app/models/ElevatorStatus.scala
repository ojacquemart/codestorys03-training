package models

import org.joda.time.DateTime
import play.api.libs.json.{JsValue, Writes, Json}
import org.joda.time.format.DateTimeFormat

case class ElevatorRecap(status: ElevatorStatus, users: UsersStatus)

object ElevatorRecap {

  implicit val elevatorStatusWriter = Json.writes[ElevatorStatus]
  implicit val usersStatusWriter = Json.writes[UsersStatus]
  implicit val writer = Json.writes[ElevatorRecap]

  def get(elevator: Elevator) = {
    new ElevatorRecap(
      ElevatorStatus.get(elevator),
      UsersStatus.get(elevator)
    )
  }
}

case class ElevatorStatus(
  hits: Int,
  floor: Int,
  cabinSize: Int,
  door: String,
  direction: String,
  lowerFloor: Int,
  higherFloor: Int,
  middleFloor: Int)

case class UsersStatus(
  total: Int,
  waiters: Int,
  travelers: Int,
  waitersByFloor: List[UserByFloor],
  travelersByFloor: List[UserByFloor])

object UsersStatus {
  def get(elevator: Elevator) = {
    new UsersStatus(
      elevator.users.size,
      elevator.users.waitersSize,
      elevator.users.travelersSize,
      elevator.users.waitersByFloor,
      elevator.users.travelersByFloor
    )
  }
}

object ElevatorStatus {

  implicit val writer = Json.writes[ElevatorStatus]

  def get(elevator: Elevator) = {
    new ElevatorStatus(
      elevator.hits,
      elevator.floor,
      elevator.cabinSize,
      elevator.door.toString,
      elevator.direction.toString,
      elevator.lowerFloor,
      elevator.higherFloor,
      elevator.middleFloor
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
