package models

import play.api.libs.json.{JsValue, Writes, Json}
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat


case class ElevatorInfo(points: Int,
                          status: ElevatorStatus,
                          resets: List[LastReset],
                          usersStatus: UsersStatus) {
  override def toString = s"""
      points=$points, door=${status.door}, direction=${status.direction}
      -
      floor=${status.floor}, higherFloor=${status.higherFloor}, lowerFloor=${status.lowerFloor}, middleFloor=${status.middleFloor}
      -
      totalUsers=${usersStatus.total}
      nbWaiters=${usersStatus.waiters}
      cabinSize=${status.cabinSize}, nbTravelers=${usersStatus.travelers}
      -
      waiters=${usersStatus.waitersByFloor.mkString(",")}
      travelers=${usersStatus.travelersByFloor.mkString(",")}
      """
}

case class ElevatorStatus(cabinSize: Int, door: String, direction: String, floor: Int, lowerFloor: Int, higherFloor: Int, middleFloor: Int)

case class UsersStatus(total: Int, waiters: Int, travelers: Int, waitersByFloor: List[Floor], travelersByFloor: List[Floor])

object ElevatorStatus {
  implicit val statusWrites = Json.writes[ElevatorStatus]
}

object ElevatorInfo {
  import ElevatorStatus.statusWrites._
  import LastReset.Writes._

  implicit val usersStatusWrites = Json.writes[UsersStatus]
  implicit val info = Json.writes[ElevatorInfo]
}

import DateTimeImplicits._

case class LastReset(cause: String, time: String = DateTime.now(), status: ElevatorStatus)

object LastReset {

  import ElevatorStatus.statusWrites

  implicit object Writes extends Writes[LastReset] {

    def writes(lastReset: LastReset): JsValue = {
      Json.obj(
        "cause" -> lastReset.cause,
        "time" -> lastReset.time,
        "elevatorStatus" -> statusWrites.writes(lastReset.status)
      )
    }
  }

}


object DateTimeImplicits {

  implicit def dateTime2String(time: DateTime): String = {
    val fmt = DateTimeFormat.forPattern("d/M/yy HH:mm:ss")

    fmt.print(time)
  }
}



